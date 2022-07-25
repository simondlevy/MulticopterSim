#[repr(C)]
pub struct VehicleState {
    x:      f32,
    dx:     f32,
    y:      f32,
    dy:     f32,
    z:      f32,
    dz:     f32,
    phi:    f32,
    dphi:   f32,
    theta:  f32,
    dtheta: f32,
    psi:    f32,
    dpsi:   f32
}

#[repr(C)]
pub struct Demands {
    throttle: f32,
    roll:     f32,
    pitch:    f32,
    yaw:      f32
}

#[repr(C)]
pub struct Motors {
    m1: f32,
    m2: f32,
    m3: f32,
    m4: f32
}

#[repr(C)]
pub struct AltHold {

    altitude_target: f32,
    error_integral:  f32,
    throttle_demand: f32
}

#[repr(C)]
pub struct Hackflight {
    motors: Motors,
    alt_hold: AltHold
}

fn in_band(value : f32, band : f32) -> bool {
    value > -band && value < band
}

fn constrain_abs(value : f32, limit : f32) -> f32 {
    if value < -limit {-limit} else if value > limit {limit} else {value}
}

fn run_alt_hold(throttle: f32, z: f32, dz: f32, pid_state: AltHold) -> AltHold {

    // Constants
    let kp = 7.5e-1;
    let ki = 1.5e0;
    let windup_max = 4.0e-1;
    let pilot_vel_z_max = 2.5e0;
    let stick_deadband = 2.0e-1;

    let inband = in_band(throttle, stick_deadband);

    let altitude = z;

    // Reset controller when moving into deadband
    let new_altitude_target =
        if inband && !in_band(pid_state.throttle_demand, stick_deadband)
        {altitude}
        else {pid_state.altitude_target};

    // Inside deadband, target velocity is difference between altitude target and current
    // altitude; outside deadband, target velocity is proportional to stick demand
    let target_velocity =
        if inband
        {new_altitude_target - altitude}
        else {pilot_vel_z_max * throttle};

    // Compute error as altTarget velocity minus actual velocity, after
    // negating actual to accommodate NED
    let error = target_velocity + dz;

    // Accumualte error integral
    let new_error_integral = constrain_abs(pid_state.error_integral + error, windup_max);

    // PI controller
    let _new_throttle_demand =  kp * error + ki * new_error_integral;

    let new_throttle_demand = if z < 1.0 {0.6} else {0.0};

    AltHold { 
        altitude_target: new_altitude_target,
        error_integral: new_error_integral,
        throttle_demand: new_throttle_demand
    }
}

fn run_hackflight(
    demands : Demands,
    vehicle_state: VehicleState,
    alt_hold: AltHold) -> Hackflight {

    let new_alt_hold = run_alt_hold(
        demands.throttle, 
        -vehicle_state.z,  // NED => ENU
        -vehicle_state.dz, // NED => ENU
        alt_hold);

    let new_throttle = new_alt_hold.throttle_demand;

    let new_motors = Motors {
        m1:new_throttle,
        m2:new_throttle,
        m3:new_throttle,
        m4:new_throttle
    };

    Hackflight {
        motors:new_motors,
        alt_hold:new_alt_hold
    }
}


#[no_mangle]
pub extern "C" fn c_run_hackflight(
    c_demands : *mut Demands,
    c_vehicle_state: *mut VehicleState,
    c_alt_hold: *mut AltHold) -> Hackflight {

    let demands = Demands {
        throttle:(unsafe { (*c_demands).throttle }),
        roll:(unsafe { (*c_demands).roll }),
        pitch:(unsafe { (*c_demands).pitch }),
        yaw:(unsafe { (*c_demands).yaw }) 
    };

    let vehicle_state = VehicleState {
        x:(unsafe { (*c_vehicle_state).x }),
        dx:(unsafe { (*c_vehicle_state).dx }),
        y:(unsafe { (*c_vehicle_state).y }),
        dy:(unsafe { (*c_vehicle_state).dy }),
        z:(unsafe { (*c_vehicle_state).z }),
        dz:(unsafe { (*c_vehicle_state).dz }),
        phi:(unsafe { (*c_vehicle_state).phi }),
        dphi:(unsafe { (*c_vehicle_state).dphi }),
        theta:(unsafe { (*c_vehicle_state).theta }),
        dtheta:(unsafe { (*c_vehicle_state).dtheta }),
        psi:(unsafe { (*c_vehicle_state).psi }),
        dpsi:(unsafe { (*c_vehicle_state).dpsi })
    };

    let alt_hold = AltHold {
        altitude_target:(unsafe { (*c_alt_hold).altitude_target }),
        error_integral:(unsafe { (*c_alt_hold).error_integral }),
        throttle_demand:(unsafe { (*c_alt_hold).throttle_demand })
    };

    run_hackflight(demands, vehicle_state, alt_hold)
}
