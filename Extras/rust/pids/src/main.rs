enum PidController {

    Altitude{r:f32},
    Angle{s:f32}
}

fn update(pid:PidController) -> (f32, PidController) {
    match pid {
        PidController::Altitude{r} => (r*r, PidController::Altitude{r:r}),
        PidController::Angle{s} => (s*s, PidController::Angle{s:s})
    }
}

fn main() {

    let c = PidController::Altitude{r:4.0};
    let s = PidController::Angle{s:2.0};

    let (x, _newc) = update(c);
    let (y, _news) = update(s);

    println!("{}, {}", x, y)
}
