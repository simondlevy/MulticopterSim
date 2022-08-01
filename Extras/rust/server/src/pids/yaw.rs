/*
   Yaw rate PID controller

   Copyright (C) 2022 Simon D. Levy

   MIT License
*/

use datatypes::datatypes::YawPid;
use datatypes::datatypes::Demands;
use datatypes::datatypes::VehicleState;

use utils::utils::fabs;
use utils::utils::constrain_abs;
use utils::utils::deg2rad;

#[repr(C)]
#[derive(Clone)]
pub struct YawPidState {
    error_integral: f32
}

pub fn run_yaw_pid_new() {

}
