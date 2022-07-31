fn fabs(value: f32) -> f32 {
    if value < 0.0 {-value} else {value}
}

fn constrain(value: f32, lo: f32, hi: f32) -> f32 {
    if value  < lo {lo} else if value > hi {hi} else {value}
}

fn constrain_abs(value : f32, limit : f32) -> f32 {
    constrain(value, -limit, limit)
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
    in_band:bool,
    target:f32
}

struct AnglePidState {
}

enum PidController {

    Altitude{state:AltHoldPidState},
    Angle{state:AnglePidState}
}

fn run_angle_pid(_angle_pid_state:AnglePidState, _demands:&Demands, _vstate:&VehicleState)
    ->  (Demands, PidController) {

        let new_demands = Demands { throttle:0.0, roll:0.0, pitch:0.0, yaw:0.0};

        let new_pid = PidController::Angle{
            state:AnglePidState {
            }
        };

        (new_demands, new_pid)
    }

fn run_alt_hold(pstate:AltHoldPidState, demands:&Demands, vstate:&VehicleState)
    ->  (Demands, PidController) {

        const KP: f32 = 0.75;
        const KI: f32 = 1.5;
        const ALTITUDE_MIN: f32   = 1.0;
        const PILOT_VELZ_MAX: f32 = 2.5;
        const STICK_DEADBAND: f32 = 0.2;
        const WINDUP_MAX: f32     = 0.4;

        // NED => ENU
        let altitude   = -vstate.z;
        let climb_rate = -vstate.dz;

        let throttle = demands.throttle;

        // Is stick demand in deadband, above a minimum altitude?
        let in_band = fabs(throttle) < STICK_DEADBAND && altitude > ALTITUDE_MIN; 

        // Zero throttle will reset error integral
        let at_zero_throttle = throttle == 0.0;

        // Reset altitude target at zero throttle
        let altitude_target = if at_zero_throttle {0.0} else {pstate.target};

        // If stick just moved into deadband, set new target altitude; otherwise,
        // keep previous
        let new_target = if in_band && !pstate.in_band {altitude} else {altitude_target};

        // Target velocity is a setpoint inside deadband, scaled
        // constant outside
        let target_velocity =
            if in_band {new_target - altitude} else {PILOT_VELZ_MAX * throttle};

        // Compute error as scaled target minus actual
        let error = target_velocity - climb_rate;

        // Compute I term, avoiding windup
        let new_error_integral = constrain_abs(pstate.error_integral + error, WINDUP_MAX);

        // Run PI controller
        let correction = error * KP + new_error_integral * KI;

        // Add correction to throttle, constraining output to [0,1]
        let new_throttle = constrain(throttle+correction, 0.0, 1.0);

        // Capture new state of PID controller
        let new_pid = PidController::Altitude{
            state:AltHoldPidState {
                error_integral:new_error_integral,
                in_band:in_band,
                target:new_target
            }
        };

        let new_demands = Demands {
            throttle:new_throttle,
            roll:demands.roll,
            pitch:demands.pitch,
            yaw:demands.yaw
        };

        (new_demands, new_pid)
    }

fn update(
    pid:PidController,
    demands:&Demands,
    vstate:&VehicleState) -> (Demands, PidController) {

    match pid {

        PidController::Altitude{state} => run_alt_hold(state, demands, vstate),

        PidController::Angle{state} => run_angle_pid(state, demands, vstate)
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
            in_band:false,
            target:0.0,
        }
    };

    let angle_pid = PidController::Angle{
        state:AnglePidState {
        }
    };

    let (_x, _new_alt_pid) = update(alt_pid, &demands, &vstate);
    let (_y, _new_angle_pid) = update(angle_pid, &demands, &vstate);

    //println!("{}, {}", x, y)
}
