use color_eyre::config::HookBuilder;
use std::{
    io::{stdin, stdout},
    panic,
    process::exit,
    sync::{Arc, Mutex},
    time::Instant,
};

use crate::{
    frame::Frame, Alloc, ErrorResponse, HandshakeRequest, HandshakeResponse, MessageRequest,
    MessageResponse, Response, Service,
};

pub fn become_child<S>(alloc: &Alloc) -> !
where
    S: Service,
{
    let start = Instant::now();

    let stdout = stdout();
    let mut stdout = stdout.lock();

    let (panic_hook, _) = HookBuilder::default().into_hooks();

    let panic_message = Arc::new(Mutex::new(String::new()));
    panic::set_hook({
        let panic_message = panic_message.clone();
        Box::new(move |info| {
            let mut panic_message = panic_message.lock().unwrap();

            let report = panic_hook.panic_report(info);
            let message = format!("{}", report);
            *panic_message = message;
        })
    });

    let mut frame = Frame::new();

    let service = {
        let stdin = stdin();
        let mut stdin = stdin.lock();

        let config = frame
            .read_sync::<HandshakeRequest<S>, _>(&mut stdin)
            .unwrap();

        let service = S::create(config);

        let (result, service) = match service {
            Ok(service) => (Ok(()), Some(service)),
            Err(err) => (Err(format!("{}", err)), None),
        };

        let response = Response {
            result,
            time_taken: Instant::now() - start,
            memory_used: alloc.get_max(),
            stdout: String::new(),
        };

        frame
            .write_sync::<HandshakeResponse, _>(&mut stdout, &response)
            .unwrap();

        match service {
            Some(service) => service,
            None => exit(1),
        }
    };

    loop {
        alloc.reset_max();

        let start = Arc::new(Mutex::new(Instant::now()));
        let result = panic::catch_unwind(|| {
            let stdin = stdin();
            let mut stdin = stdin.lock();

            let mut frame = Frame::new();
            let request = frame.read_sync::<MessageRequest<S>, _>(&mut stdin).unwrap();

            // Start the timer once the request has been read.
            *start.lock().unwrap() = Instant::now();

            service.handle(request)
        })
        .map_err(|_| ErrorResponse::Panic(panic_message.lock().unwrap().clone()));

        let memory_used = alloc.get_max();
        let time_taken = Instant::now() - *start.lock().unwrap();
        let should_exit = result.is_err();

        let response = Response {
            result,
            memory_used,
            time_taken,
            stdout: String::new(),
        };

        frame
            .write_sync::<MessageResponse<S>, _>(&mut stdout, &response)
            .unwrap();

        // It's difficult to recover from a panic, so just exit gracefully and have a new process start up.
        if should_exit {
            exit(1);
        }
    }
}
