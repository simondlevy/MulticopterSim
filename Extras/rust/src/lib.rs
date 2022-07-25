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

fn alt_hold(demands : Demands, vstate : VehicleState) -> Demands {

    // Constants
    let _kp = 7.5e-1;
    let _ki = 1.5e0;
    let _windup_max = 4.0e-1;
    let _pilot_vel_z_max = 2.5e0;
    let stick_deadband = 2.0e-1;

    // State variables
    let mut _error_integral = 0.0;
    let mut _throttle_demand = 0.0; 
    let mut _altitude_target = 0.0; 

    let _inband = in_band(demands.throttle, stick_deadband);

    // NED => ENU
    let altitude = -vstate.z; 

    let new_throttle = if altitude < 1.0 { 0.6 } else { 0.0 };

    Demands { throttle:new_throttle,
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
