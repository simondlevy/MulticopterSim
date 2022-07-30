struct VehicleState {

    //pub z:      f32,
    //pub dz:     f32,
    //pub dphi:   f32,
    //pub dtheta: f32,
    //pub dpsi:   f32
}

struct Demands {

    //pub throttle: f32,
    //pub roll:     f32,
    //pub pitch:    f32,
    //pub yaw:      f32
} 

enum PidController {

    Altitude{r:f32},
    Angle{s:f32}
}

fn update(
    pid:PidController,
    _demands:&Demands,
    _vstate:&VehicleState) -> (f32, PidController) {

    match pid {

        PidController::Altitude{r} =>
            (r*r, PidController::Altitude{r:r}),

        PidController::Angle{s} =>
            (s*s, PidController::Angle{s:s})
    }
}

fn main() {

    let demands = Demands {
        //throttle:0.0,
        //roll:0.0,
        //pitch:0.0,
        //yaw:0.0
    };

    let vstate = VehicleState {
        //z:0.0,
        //dz:0.0,
        //dphi:0.0,
        //dtheta:0.0,
        //dpsi:0.0
    };

    let alt_pid = PidController::Altitude{r:4.0};
    let angle_pid = PidController::Angle{s:2.0};

    let (_x, _new_alt_pid) = update(alt_pid, &demands, &vstate);
    let (_y, _new_angle_pid) = update(angle_pid, &demands, &vstate);

    //println!("{}, {}", x, y)
}
