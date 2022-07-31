fn fabs(value: f32) -> f32 {
    if value < 0.0 {-value} else {value}
}

struct VehicleState {

    pub z:      f32,
    pub dz:     f32,
    //pub dphi:   f32,
    //pub dtheta: f32,
    //pub dpsi:   f32
}

struct Demands {

    pub throttle: f32,
    pub roll:     f32,
    pub pitch:    f32,
    pub yaw:      f32
} 

struct AltHoldPidState {
    error_integral:f32,
    target:f32
}

struct AnglePidState {
    last_dyn_lpf_update_us:u32
}

enum PidController {

    Altitude{state:AltHoldPidState},
    Angle{state:AnglePidState}
}

fn run_angle_pid(angle_pid_state:AnglePidState, _demands:&Demands, _vstate:&VehicleState)
    ->  (Demands, PidController) {

        let new_demands = Demands { throttle:0.0, roll:0.0, pitch:0.0, yaw:0.0};

        let new_pid = PidController::Angle{
            state:AnglePidState {
                last_dyn_lpf_update_us:0
            }
        };

        (new_demands, new_pid)
    }

fn run_alt_hold(alt_hold_pid_state:AltHoldPidState, _demands:&Demands, _vstate:&VehicleState)
    ->  (Demands, PidController) {

        let new_demands = Demands { throttle:0.0, roll:0.0, pitch:0.0, yaw:0.0};

        let new_pid = PidController::Altitude{
            state:AltHoldPidState {
                error_integral:0.0,
                target:0.0,
            }
        };

        (new_demands, new_pid)
    }

fn update(
    pid:PidController,
    demands:&Demands,
    vstate:&VehicleState) -> (Demands, PidController) {

    match pid {

        PidController::Altitude{state: state} =>
            run_alt_hold(state, demands, vstate),

            PidController::Angle{state: state} =>
                run_angle_pid(state, demands, vstate)
    }
}

fn main() {

    let demands = Demands {
        throttle:0.0,
        roll:0.0,
        pitch:0.0,
        yaw:0.0
    };

    let vstate = VehicleState {
        z:0.0,
        dz:0.0,
        //dphi:0.0,
        //dtheta:0.0,
        //dpsi:0.0
    };

    let alt_pid = PidController::Altitude{
        state:AltHoldPidState {
            error_integral:0.0,
            target:0.0,
        }
    };

    let angle_pid = PidController::Angle{
        state:AnglePidState {
            last_dyn_lpf_update_us:0
        }
    };

    let (_x, _new_alt_pid) = update(alt_pid, &demands, &vstate);
    let (_y, _new_angle_pid) = update(angle_pid, &demands, &vstate);

    //println!("{}, {}", x, y)
}
