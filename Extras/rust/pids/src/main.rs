enum PidController {

    Altitude{r:f32},
    Angle{s:f32}
}

fn update(pid:PidController) -> f32 {
    match pid {
        PidController::Altitude{r} => r*r,
        PidController::Angle{s} => s*s
    }
}

fn main() {

    let c = PidController::Altitude{r:4.0};
    let s = PidController::Angle{s:2.0};

    let x = update(c);
    let y = update(s);

    println!("{}, {}", x, y)
}
