use crate::error::ErrorResponse;
use crate::response::Response;
use crate::Error;

pub(crate) enum Message<Res> {
    Response(Response<Res>),
    ErrorResponse(ErrorResponse),
    RecvError(Error),
    Interrupt,
}
