extern crate listenfd;
use listenfd::ListenFd;

extern crate actix_web;
use actix_web::{App, server, HttpRequest, Result, http::Method, fs::NamedFile};


pub fn index(_req: HttpRequest) -> Result<NamedFile> {
    Ok(NamedFile::open("public/index.html")?)
}

pub fn bundle(_req: HttpRequest) -> Result<NamedFile> {
    Ok(NamedFile::open("public/bundle.js")?)
}

fn main() {
    let mut listenfd = ListenFd::from_env();
    let mut server = server::new(|| App::new()
            .resource("/", |r| r.method(Method::GET).f(index))
            .resource("/bundle.js", |r| r.method(Method::GET).f(bundle))
            );

    server = if let Some(l) = listenfd.take_tcp_listener(0).unwrap() {
        server.listen(l)
    } else {
        server.bind("127.0.0.1:3000").unwrap()
    };

    server.run();
}