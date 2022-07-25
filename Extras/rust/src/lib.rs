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

fn in_band(value : f32, band : f32) -> bool {
    value > -band && value < band
}

fn constrain_abs(value : f32, limit : f32) -> f32 {
    if value < -limit {-limit} else if value > limit {limit} else {value}
}

fn alt_hold(demands : Demands, vstate : VehicleState) -> Demands {

    // Constants
    let kp = 7.5e-1;
    let ki = 1.5e0;
    let windup_max = 4.0e-1;
    let pilot_vel_z_max = 2.5e0;
    let stick_deadband = 2.0e-1;

    // State variables
    let mut error_integral_prev = 0.0;
    let mut throttle_demand_prev = 0.0; 
    let mut altitude_target_prev = 0.0; 

    let inband = in_band(demands.throttle, stick_deadband);

    // NED => ENU
    let altitude = -vstate.z; 

    // Reset controller when moving into deadband
    let altitude_target =
        if inband && !in_band(throttle_demand_prev, stick_deadband)
        {altitude}
        else {altitude_target_prev};

    // Inside deadband, target velocity is difference between altitude target and current
    // altitude; outside deadband, target velocity is proportional to stick demand
    let target_velocity =
        if inband
        {altitude_target - altitude}
        else {pilot_vel_z_max * demands.throttle};

    // Compute error as altTarget velocity minus actual velocity, after
    // negating actual to accommodate NED
    let error = target_velocity + vstate.dz;

    // Accumualte error integral
    let error_integral = constrain_abs(error_integral_prev + error, windup_max);

    // PI controller
    let throttle_demand =  kp * error + ki * error_integral;

    // Update state variables
    error_integral_prev = error_integral;
    throttle_demand_prev = throttle_demand;
    altitude_target_prev = altitude_target;

    Demands { throttle:throttle_demand,
              roll:demands.roll,
              pitch:demands.pitch,
              yaw:demands.yaw
    }
}

#[no_mangle]
pub extern "C" fn get_motors(
    c_demands : *mut Demands, c_vstate: *mut VehicleState) -> Motors {

    let demands = Demands {
        throttle:(unsafe { (*c_demands).throttle }),
        roll:(unsafe { (*c_demands).roll }),
        pitch:(unsafe { (*c_demands).pitch }),
        yaw:(unsafe { (*c_demands).yaw }) 
    };

    let vstate = VehicleState {
        x:(unsafe { (*c_vstate).x }),
        dx:(unsafe { (*c_vstate).dx }),
        y:(unsafe { (*c_vstate).y }),
        dy:(unsafe { (*c_vstate).dy }),
        z:(unsafe { (*c_vstate).z }),
        dz:(unsafe { (*c_vstate).dz }),
        phi:(unsafe { (*c_vstate).phi }),
        dphi:(unsafe { (*c_vstate).dphi }),
        theta:(unsafe { (*c_vstate).theta }),
        dtheta:(unsafe { (*c_vstate).dtheta }),
        psi:(unsafe { (*c_vstate).psi }),
        dpsi:(unsafe { (*c_vstate).dpsi })
    };

    let new_demands = alt_hold(demands, vstate);

    let throttle = new_demands.throttle;

    Motors { m1:throttle, m2:throttle, m3:throttle, m4:throttle }
}
