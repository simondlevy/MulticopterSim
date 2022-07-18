::
::  Build script for Rust DLL
::
:: Copyright (C) 2022 Simon D. Levy
::
:: MIT License

cargo build
copy target\debug\vec.dll ..\..\Binaries\Win64
