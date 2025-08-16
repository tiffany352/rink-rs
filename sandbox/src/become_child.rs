// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use color_eyre::config::HookBuilder;
use std::alloc::GlobalAlloc;
use std::io::{stdin, stdout};
use std::process::exit;
use std::sync::{Arc, Mutex};
use std::time::Instant;

use crate::{
    error::ErrorResponse,
    frame::Frame,
    response::Response,
    service::{HandshakeRequest, HandshakeResponse, MessageRequest, MessageResponse, Service},
    Alloc,
};

/// When your app is passed the arguments returned by [`Service::args`],
/// it should call this function to begin servicing requests.
pub fn become_child<S, A>(alloc: &Alloc<A>) -> !
where
    S: Service,
    A: GlobalAlloc,
{
    let start = Instant::now();

    let stdout = stdout();
    let mut stdout = stdout.lock();

    let (panic_hook, _) = HookBuilder::default().into_hooks();

    let panic_message = Arc::new(Mutex::new(String::new()));
    std::panic::set_hook({
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
        let result = std::panic::catch_unwind(|| {
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
