use abi_stable::{
    std_types::{
        RBoxError,
        RResult::{self},
        RString,
    },
    RMut,
};
use alacritty_terminal::{
    event::{Event, EventListener, OnResize, WindowSize},
    grid::Dimensions,
    index::{Column, Line as GridLine, Point},
    term::{
        cell::{Cell, Flags},
        Config, Term,
    },
    tty::{new as new_tty, EventedPty, EventedReadWrite, Options, Pty, Shell},
    vte::ansi::{Color, NamedColor, Processor, Rgb},
};
use async_io::Timer;
use futures_channel::mpsc::{unbounded, UnboundedReceiver};
use futures_util::{
    future::{select, Either},
    lock::Mutex as AsyncMutex,
    pin_mut, FutureExt,
};
use parking_lot::Mutex;
use rustix::process::{kill_process, Pid, Signal};
use std::{
    io::{ErrorKind, Read, Write},
    sync::{
        mpsc::{channel, Sender, TryRecvError},
        Arc,
    },
    thread::{sleep, spawn},
    time::Duration,
};
use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{
        as_underlying_ffi_type, CustomRef, FFIArg, FFIModule, FFIValue, FfiFuture, FfiFutureExt,
        IntoFFIVal, RegisterFFIFn, VectorRef,
    },
};

// Note: This is no bueno, but we'll need this for now
// until we figure out how to relax some of the constraints. I'm guessing
// that the FFI layer probably just needs to use Mutex instead of RwLock
// to avoid the Sync problem.
unsafe impl Send for PtyProcess {}
unsafe impl Sync for PtyProcess {}

unsafe impl Send for VirtualTerminal {}
unsafe impl Sync for VirtualTerminal {}

#[derive(Clone)]
struct PtyEventListener {
    writer: Option<Arc<Mutex<Pty>>>,
}

impl EventListener for PtyEventListener {
    fn send_event(&self, event: Event) {
        match event {
            Event::PtyWrite(text) => {
                if let Some(pty) = self.writer.as_ref() {
                    let mut pty = pty.lock();
                    let writer = pty.writer();
                    if writer.write_all(text.as_bytes()).is_ok() {
                        let _ = writer.flush();
                    }
                }
            }
            Event::ClipboardLoad(_, formatter) => {
                if let Some(pty) = self.writer.as_ref() {
                    let mut pty = pty.lock();
                    let writer = pty.writer();
                    if writer.write_all(formatter("").as_bytes()).is_ok() {
                        let _ = writer.flush();
                    }
                }
            }
            Event::ColorRequest(_, formatter) => {
                if let Some(pty) = self.writer.as_ref() {
                    let mut pty = pty.lock();
                    let writer = pty.writer();
                    if writer
                        .write_all(formatter(Default::default()).as_bytes())
                        .is_ok()
                    {
                        let _ = writer.flush();
                    }
                }
            }
            _ => {}
        }
    }
}

struct PtyProcess {
    cancellation_token_sender: Sender<()>,
    command_sender: Sender<u8>,
    async_receiver: Arc<AsyncMutex<UnboundedReceiver<String>>>,
    pty: Arc<Mutex<Pty>>,
    writer: Option<Arc<Mutex<Pty>>>,
}

impl PtyProcess {
    pub fn kill(&mut self) {
        if let Some(pid) = Pid::from_raw(self.pty.lock().child().id() as i32) {
            let _ = kill_process(pid, Signal::HUP);
        }
        let _ = self.cancellation_token_sender.send(());
    }

    // TODO: Replace this with a proper result rather than a bool
    pub fn send_command_char(&mut self, command: char) -> bool {
        self.command_sender.send(command as u8).is_ok_and(|_| true)
    }

    // TODO: Replace this with a proper result rather than a bool
    pub fn send_command(&mut self, command: &str) -> bool {
        for byte in command.as_bytes() {
            if self.command_sender.send(*byte).is_err() {
                return false;
            }
        }

        true
    }

    // TODO: rows + cols should be u16,
    // and those bounds checks should be implemented on
    // the conversion
    pub fn resize(&mut self, rows: usize, cols: usize) -> RResult<FFIValue, RBoxError> {
        self.pty.lock().on_resize(WindowSize {
            num_lines: rows as u16,
            num_cols: cols as u16,
            cell_width: 0,
            cell_height: 0,
        });
        RResult::ROk(FFIValue::Void)
    }

    // Attempt to move the bytes without cloning the heap allocation underneath?
    pub fn async_try_read_line(&mut self) -> FfiFuture<RResult<FFIValue, RBoxError>> {
        let ar = Arc::clone(&self.async_receiver);

        async move {
            let mut guard = ar.lock().await;

            let mut buffer = String::new();

            // Optimistically read as much as we can into this buffer.
            // Yield back once we have nothing else.
            while let Ok(v) = guard.try_recv() {
                buffer.push_str(&v);
            }

            let next = guard.recv();
            let timeout = Timer::after(Duration::from_millis(2));

            pin_mut!(next);

            match select(next, timeout).await {
                Either::Left((x, _)) => {
                    if let Ok(message) = x {
                        buffer.push_str(&message);
                        RResult::ROk(FFIValue::StringV(buffer.into()))
                    } else if buffer.is_empty() {
                        RResult::ROk(FFIValue::BoolV(false))
                    } else {
                        RResult::ROk(FFIValue::StringV(buffer.into()))
                    }
                }
                Either::Right((_, fut)) => {
                    if buffer.is_empty() {
                        fut.map(|x| {
                            if let Ok(message) = x {
                                buffer.push_str(&message);

                                RResult::ROk(FFIValue::StringV(buffer.into()))
                            } else {
                                RResult::ROk(FFIValue::BoolV(false))
                            }
                        })
                        .await
                    } else {
                        RResult::ROk(FFIValue::StringV(buffer.into()))
                    }
                }
            }
        }
        .into_ffi()
    }
}

impl Custom for PtyProcess {}

impl Drop for PtyProcess {
    fn drop(self: &mut PtyProcess) {
        self.kill();
    }
}

declare_module!(create_module);

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("steel/pty-process");

    module
        .register_fn("create-native-pty-system!", create_native_pty_system)
        .register_fn("kill-pty-process!", PtyProcess::kill)
        .register_fn("pty-process-send-command", PtyProcess::send_command)
        .register_fn(
            "pty-process-send-command-char",
            PtyProcess::send_command_char,
        )
        .register_fn("async-try-read-line", PtyProcess::async_try_read_line)
        .register_fn("pty-resize!", PtyProcess::resize)
        .register_fn("virtual-terminal", |pty: &mut PtyProcess| VirtualTerminal {
            terminal: Term::new(
                Config::default(),
                &TerminalDimensions::new(80, 24),
                PtyEventListener {
                    writer: pty.writer.clone(),
                },
            ),
            parser: Processor::new(),
            screen_iterator: ScreenCellIterator { x: 0, y: 0 },
            last_cell: None,
            scroll_up_modifier: 0,
        })
        // Raw virtual terminal!
        .register_fn("raw-virtual-terminal", || VirtualTerminal {
            terminal: Term::new(
                Config::default(),
                &TerminalDimensions::new(80, 24),
                PtyEventListener { writer: None },
            ),
            parser: Processor::new(),
            screen_iterator: ScreenCellIterator { x: 0, y: 0 },
            last_cell: None,
            scroll_up_modifier: 0,
        })
        .register_fn("vte/advance-bytes", VirtualTerminal::advance_bytes)
        // Advancing with immediate action
        .register_fn(
            "vte/advance-bytes-char",
            VirtualTerminal::advance_bytes_char,
        )
        .register_fn(
            "vte/advance-bytes-with-carriage-return",
            VirtualTerminal::advance_bytes_with_carriage_return,
        )
        .register_fn("vte/resize", VirtualTerminal::resize)
        .register_fn("vte/lines", VirtualTerminal::lines)
        .register_fn("vte/line->string", TermLine::as_str)
        .register_fn("vte/cursor", VirtualTerminal::cursor)
        .register_fn("vte/cursor-x", VirtualTerminal::cursor_x)
        .register_fn("vte/cursor-y", VirtualTerminal::cursor_y)
        .register_fn("vte/line->cells", |line: &mut TermLine| -> Vec<FFIValue> {
            line.cells
                .iter()
                .cloned()
                .map(|cell| TermCell { cell }.into_ffi_val().unwrap())
                .collect()
        })
        .register_fn("vte/cell->fg", |cell: &TermCell| {
            TermColorAttribute(cell.cell.fg)
        })
        .register_fn("vte/cell->bg", |cell: &TermCell| {
            TermColorAttribute(cell.cell.bg)
        })
        // Get the color attribute, map it to the one that helix uses
        // TODO: Re-use the memory - we should pass in an FFI Vector, and then just reuse it over and over.
        .register_fn(
            "term/color-attribute",
            |attribute: &TermColorAttribute| match attribute.0 {
                Color::Spec(Rgb { r, g, b }) => vec![
                    (r as isize).into_ffi_val().unwrap(),
                    (g as isize).into_ffi_val().unwrap(),
                    (b as isize).into_ffi_val().unwrap(),
                    255isize.into_ffi_val().unwrap(),
                ]
                .into_ffi_val(),
                Color::Indexed(index) => (index as usize).into_ffi_val(),
                Color::Named(NamedColor::Foreground | NamedColor::Background) => {
                    false.into_ffi_val()
                }
                Color::Named(color) => (named_color_index(color) as usize).into_ffi_val(),
            },
        )
        .register_fn(
            "term/color-attribute-set!",
            |attribute: &TermColorAttribute, shared_vec: FFIArg| {
                if let FFIArg::VectorRef(VectorRef { mut vec, .. }) = shared_vec {
                    match attribute.0 {
                        Color::Spec(Rgb { r, g, b }) => {
                            vec[0] = FFIValue::IntV(r as isize);
                            vec[1] = FFIValue::IntV(g as isize);
                            vec[2] = FFIValue::IntV(b as isize);
                            vec[3] = FFIValue::IntV(255);

                            true.into_ffi_val()
                        }
                        Color::Indexed(index) => (index as usize).into_ffi_val(),
                        Color::Named(NamedColor::Foreground | NamedColor::Background) => {
                            false.into_ffi_val()
                        }
                        Color::Named(color) => (named_color_index(color) as usize).into_ffi_val(),
                    }
                } else {
                    false.into_ffi_val()
                }
            },
        )
        .register_fn("vte/cell-width", |cell: &TermCell| cell_width(&cell.cell))
        .register_fn("vte/cell-string", |cell: &TermCell| cell_string(&cell.cell))
        .register_fn("vte/reset-iterator!", |term: &mut VirtualTerminal| {
            term.screen_iterator.x = 0;
            term.screen_iterator.y = term.scroll_up_modifier;
            term.last_cell = None;
        })
        // This should move forward until we actually have something meaningful,
        // rather than arbitrarily stepping forward one step.
        .register_fn("vte/advance-iterator!", |term: &mut VirtualTerminal| {
            let (rows, cols) = term.iteration_bounds();

            if term.screen_iterator.x < cols && term.screen_iterator.y < rows {
                term.last_cell = term.cell_at_iterator().cloned();

                term.screen_iterator.x += 1;
                return true;
            }

            if term.screen_iterator.x >= cols && term.screen_iterator.y < rows {
                term.screen_iterator.x = 0;
                term.screen_iterator.y += 1;
                term.last_cell = None;

                return true;
            }

            false
        })
        // Advance until there is a string?
        .register_fn(
            "vte/advance-iterator-until-string!",
            |term: &mut VirtualTerminal| {
                let (rows, cols) = term.iteration_bounds();

                loop {
                    if term.screen_iterator.x < cols && term.screen_iterator.y < rows {
                        let last_cell = term.cell_at_iterator().cloned();

                        term.screen_iterator.x += 1;

                        if last_cell.is_some() {
                            term.last_cell = last_cell;
                            return true;
                        }
                        continue;
                    }

                    if term.screen_iterator.x >= cols && term.screen_iterator.y < rows {
                        term.screen_iterator.x = 0;
                        term.screen_iterator.y += 1;
                        continue;
                    }
                    return false;
                }
            },
        )
        .register_fn(
            "vte/advance-iterator-and-update-cells!",
            |term: &mut VirtualTerminal, mut_str: RMut<'_, RString>, bg: FFIArg, fg: FFIArg| {
                let (rows, cols) = term.iteration_bounds();

                loop {
                    if term.screen_iterator.x < cols && term.screen_iterator.y < rows {
                        let last_cell = term.cell_at_iterator().cloned();

                        term.screen_iterator.x += 1;

                        if let Some(cell) = last_cell {
                            // term.last_cell = last_cell.cloned();
                            update_cell(&cell, mut_str, bg, fg);

                            return true;
                        }
                        continue;
                    }

                    if term.screen_iterator.x >= cols && term.screen_iterator.y < rows {
                        term.screen_iterator.x = 0;
                        term.screen_iterator.y += 1;
                        continue;
                    }
                    return false;
                }
            },
        )
        .register_fn("vte/iter-x", |term: &VirtualTerminal| {
            term.screen_iterator.x
        })
        .register_fn("vte/iter-y", |term: &VirtualTerminal| {
            (term.screen_iterator.y - term.scroll_up_modifier) as isize
        })
        // TODO: Add function to mutate in place
        .register_fn("vte/iter-cell-fg", |term: &VirtualTerminal| {
            if let Some(cell) = &term.last_cell {
                TermColorAttribute(cell.fg).into_ffi_val()
            } else {
                false.into_ffi_val()
            }
        })
        // TODO: Add function to mutate in place
        .register_fn("vte/iter-cell-bg", |term: &VirtualTerminal| {
            if let Some(cell) = &term.last_cell {
                TermColorAttribute(cell.bg).into_ffi_val()
            } else {
                false.into_ffi_val()
            }
        })
        .register_fn("vte/empty-cell", || {
            TermColorAttribute(Color::Named(NamedColor::Foreground))
        })
        .register_fn(
            "vte/iter-cell-bg-fg-set-attr!",
            |term: &VirtualTerminal, bg: FFIArg, fg: FFIArg| {
                if let Some(cell) = &term.last_cell {
                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = bg {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.bg;
                        }
                    }

                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = fg {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.fg;

                            return true.into_ffi_val();
                        }
                        return false.into_ffi_val();
                    }
                    return false.into_ffi_val();
                }

                true.into_ffi_val()
            },
        )
        .register_fn(
            "vte/iter-cell-bg-set-attr!",
            |term: &VirtualTerminal, val: FFIArg| {
                if let Some(cell) = &term.last_cell {
                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = val {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.bg;

                            true.into_ffi_val()
                        } else {
                            false.into_ffi_val()
                        }
                    } else {
                        false.into_ffi_val()
                    }
                } else {
                    false.into_ffi_val()
                }
            },
        )
        .register_fn(
            "vte/iter-cell-fg-set-attr!",
            |term: &VirtualTerminal, val: FFIArg| {
                if let Some(cell) = &term.last_cell {
                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = val {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.fg;

                            true.into_ffi_val()
                        } else {
                            false.into_ffi_val()
                        }
                    } else {
                        false.into_ffi_val()
                    }
                } else {
                    false.into_ffi_val()
                }
            },
        )
        .register_fn("vte/iter-cell-str", |term: &VirtualTerminal| {
            if let Some(cell) = &term.last_cell {
                cell_string(cell).into_ffi_val()
            } else {
                false.into_ffi_val()
            }
        })
        .register_fn(
            "vte/iter-cell-str-set-str!",
            |term: &VirtualTerminal, mut mut_str: RMut<'_, RString>| {
                if let Some(cell) = &term.last_cell {
                    mut_str.get_mut().clear();
                    mut_str.get_mut().push_str(&cell_string(cell));

                    RResult::ROk(FFIValue::Void)
                } else {
                    false.into_ffi_val()
                }
            },
        )
        // Batch all of the updates, less round trips across the FFI barrier
        .register_fn(
            "vte/iter-cell-bg-fg-set-attr-str!",
            |term: &VirtualTerminal, mut mut_str: RMut<'_, RString>, bg: FFIArg, fg: FFIArg| {
                if let Some(cell) = &term.last_cell {
                    mut_str.get_mut().clear();
                    mut_str.get_mut().push_str(&cell_string(cell));

                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = bg {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.bg;
                        }
                    }

                    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = fg {
                        if let Some(attr) =
                            as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut())
                        {
                            attr.0 = cell.fg;

                            return true.into_ffi_val();
                        }
                        return false.into_ffi_val();
                    }
                    return false.into_ffi_val();
                }

                true.into_ffi_val()
            },
        )
        .register_fn("vte/scroll-up", |term: &mut VirtualTerminal| {
            term.scroll_up_modifier =
                (term.scroll_up_modifier - 1).max(0 - term.terminal.history_size() as i64);
        })
        .register_fn("vte/scroll-down", |term: &mut VirtualTerminal| {
            term.scroll_up_modifier = (term.scroll_up_modifier + 1).min(0);
        });

    module
}

fn cell_string(cell: &Cell) -> String {
    if cell.flags.contains(Flags::WIDE_CHAR_SPACER)
        || cell.flags.contains(Flags::LEADING_WIDE_CHAR_SPACER)
    {
        String::new()
    } else {
        cell.c.to_string()
    }
}

fn cell_width(cell: &Cell) -> usize {
    if cell.flags.contains(Flags::WIDE_CHAR) {
        2
    } else if cell.flags.contains(Flags::WIDE_CHAR_SPACER)
        || cell.flags.contains(Flags::LEADING_WIDE_CHAR_SPACER)
    {
        0
    } else {
        1
    }
}

fn named_color_index(color: NamedColor) -> u8 {
    match color {
        NamedColor::Black => 0,
        NamedColor::Red => 1,
        NamedColor::Green => 2,
        NamedColor::Yellow => 3,
        NamedColor::Blue => 4,
        NamedColor::Magenta => 5,
        NamedColor::Cyan => 6,
        NamedColor::White => 7,
        NamedColor::BrightBlack => 8,
        NamedColor::BrightRed => 9,
        NamedColor::BrightGreen => 10,
        NamedColor::BrightYellow => 11,
        NamedColor::BrightBlue => 12,
        NamedColor::BrightMagenta => 13,
        NamedColor::BrightCyan => 14,
        NamedColor::BrightWhite => 15,
        _ => 0,
    }
}

fn update_cell(cell: &Cell, mut mut_str: RMut<'_, RString>, bg: FFIArg, fg: FFIArg) {
    mut_str.get_mut().clear();
    mut_str.get_mut().push_str(&cell_string(cell));

    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = bg {
        if let Some(attr) = as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut()) {
            attr.0 = cell.bg;
        }
    }

    if let FFIArg::CustomRef(CustomRef { mut custom, .. }) = fg {
        if let Some(attr) = as_underlying_ffi_type::<TermColorAttribute>(custom.get_mut()) {
            attr.0 = cell.fg;
        }
    }
}

struct TermColorAttribute(Color);

impl Custom for TermColorAttribute {}

struct TerminalDimensions {
    columns: usize,
    screen_lines: usize,
}

impl TerminalDimensions {
    fn new(columns: usize, screen_lines: usize) -> Self {
        Self {
            columns,
            screen_lines,
        }
    }
}

impl Dimensions for TerminalDimensions {
    fn total_lines(&self) -> usize {
        self.screen_lines
    }

    fn screen_lines(&self) -> usize {
        self.screen_lines
    }

    fn columns(&self) -> usize {
        self.columns
    }
}

fn create_native_pty_system(command: String) -> PtyProcess {
    let pty = Mutex::new(
        new_tty(
            &Options {
                shell: Some(Shell::new(command, Vec::new())),
                ..Options::default()
            },
            WindowSize {
                num_lines: 24,
                num_cols: 80,
                cell_width: 0,
                cell_height: 0,
            },
            0,
        )
        .unwrap(),
    )
    .into();

    // Read the output in another thread.
    // This is important because it is easy to encounter a situation
    // where read/write buffers fill and block either your process
    // or the spawned process.
    let (async_sender, async_receiver) = unbounded();

    let (cancellation_token_sender, cancellation_token_receiver) = channel();

    let reader_pty = Arc::clone(&pty);
    let writer = Some(pty.clone());

    let writer_clone = writer.clone();

    spawn(move || {
        // Consume the output from the child
        let mut read_buffer = [0; 65536];

        loop {
            match reader_pty.lock().reader().read(&mut read_buffer) {
                Ok(size) => {
                    if size == 0 {
                        break;
                    }
                    if async_sender
                        .unbounded_send(String::from_utf8_lossy(&read_buffer[..size]).into())
                        .is_err()
                    {
                        break;
                    }
                }
                Err(e) => {
                    if !matches!(e.kind(), ErrorKind::WouldBlock | ErrorKind::Interrupted) {
                        break;
                    }
                }
            }

            {
                let mut pty = reader_pty.lock();
                if pty.next_child_event().is_some() {
                    break;
                }
            }

            if let Err(e) = cancellation_token_receiver.try_recv() {
                if matches!(e, TryRecvError::Disconnected) {
                    break;
                }
            } else {
                break;
            }
        }
    });

    // TODO: Perhaps, don't use Strings here and instead just
    // use byte strings directly. However I think Strings work
    // just find for now.
    let (command_sender, command_receiver) = channel();

    {
        // Obtain the writer.
        // When the writer is dropped, EOF will be sent to
        // the program that was spawned.
        // It is important to take the writer even if you don't
        // send anything to its stdin so that EOF can be
        // generated, otherwise you risk deadlocking yourself.
        // let mut writer = pair.master.take_writer().unwrap();

        if cfg!(target_os = "macos") {
            // macOS quirk: the child and reader must be started and
            // allowed a brief grace period to run before we allow
            // the writer to drop. Otherwise, the data we send to
            // the kernel to trigger EOF is interleaved with the
            // data read by the reader! WTF!?
            // This appears to be a race condition for very short
            // lived processes on macOS.
            // I'd love to find a more deterministic solution to
            // this than sleeping.
            sleep(Duration::from_millis(20));
        }

        // This example doesn't need to write anything, but if you
        // want to send data to the child, you'd set `to_write` to
        // that data and do it like this:
        // let to_write = "ls -l";
        // if !to_write.is_empty() {
        // To avoid deadlock, wrt. reading and waiting, we send
        // data to the stdin of the child in a different thread.
        spawn(move || {
            while let Ok(command) = command_receiver.recv() {
                if let Some(w) = writer.as_ref() {
                    w.lock().writer().write_all(&[command]).unwrap();
                }
            }
        });
    }

    PtyProcess {
        cancellation_token_sender,
        command_sender,
        async_receiver: AsyncMutex::new(async_receiver).into(),
        pty,
        writer: writer_clone,
    }
}

// Have this virtual terminal receive
// inputs from the plugin, where the plugin
// then does the rendering logic.
// Get back this terminal in a way that rendering
// is reasonably easy.
struct VirtualTerminal {
    terminal: Term<PtyEventListener>,
    parser: Processor,
    screen_iterator: ScreenCellIterator,
    scroll_up_modifier: i64,
    last_cell: Option<Cell>,
}

struct ScreenCellIterator {
    x: usize,
    y: i64,
}

impl Custom for VirtualTerminal {}

struct TermCell {
    cell: Cell,
}

impl Custom for TermCell {}

struct TermLine {
    cells: Vec<Cell>,
}

impl TermLine {
    // Convert the line to a string
    fn as_str(&self) -> String {
        self.cells.iter().map(cell_string).collect()
    }
}

impl Custom for TermLine {}

impl VirtualTerminal {
    // Keep track of the state of the terminal
    fn advance_bytes(&mut self, bytes: &str) {
        self.parser.advance(&mut self.terminal, bytes.as_bytes());
    }

    fn advance_bytes_char(&mut self, bytes: char) {
        let mut buf = [0; 4];
        self.advance_bytes(bytes.encode_utf8(&mut buf));
    }

    fn advance_bytes_with_carriage_return(&mut self, bytes: &str) {
        let mut buf = [0; 4];

        for char in bytes.chars() {
            match char {
                '\n' => self.advance_bytes("\n\r"),
                c => self.advance_bytes(c.encode_utf8(&mut buf)),
            }
        }
    }

    // Resizes the terminal
    fn resize(&mut self, rows: usize, cols: usize) {
        self.terminal.resize(TerminalDimensions::new(cols, rows));
    }

    // Get the content to render
    fn lines(&mut self) -> Vec<FFIValue> {
        let grid = self.terminal.grid();

        (0..self.terminal.screen_lines())
            .map(|row| {
                let line = GridLine(row as i32);
                let cells = (0..self.terminal.columns())
                    .map(|col| grid[Point::new(line, Column(col))].clone())
                    .collect();
                TermLine { cells }.into_ffi_val().unwrap()
            })
            .collect()
    }

    fn cursor(&self) -> Vec<FFIValue> {
        let pos = self.terminal.grid().cursor.point;

        vec![
            pos.column.0.into_ffi_val().unwrap(),
            (pos.line.0 as isize).into_ffi_val().unwrap(),
        ]
    }

    fn cursor_x(&self) -> usize {
        self.terminal.grid().cursor.point.column.0
    }

    fn cursor_y(&self) -> isize {
        self.terminal.grid().cursor.point.line.0 as isize
    }

    fn iteration_bounds(&self) -> (i64, usize) {
        (
            self.terminal.screen_lines() as i64 + self.scroll_up_modifier,
            self.terminal.columns(),
        )
    }

    fn cell_at_iterator(&self) -> Option<&Cell> {
        let line = GridLine(self.screen_iterator.y as i32);
        let column = Column(self.screen_iterator.x);
        let topmost = self.terminal.topmost_line().0;
        let bottommost = self.terminal.bottommost_line().0;

        (column.0 < self.terminal.columns() && line.0 >= topmost && line.0 <= bottommost)
            .then(|| &self.terminal.grid()[Point::new(line, column)])
    }
}
