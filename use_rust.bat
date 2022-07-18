::
:: Socket module setup script for MulticopterSim
::
:: Copyright (C) 2021 Simon D. Levy
::
:: MIT License

copy Extras\modules\rust\MulticopterSim_rust.uproject MulticopterSim.uproject
copy Extras\modules\rust\MulticopterSim_rust.Target.cs Source\MulticopterSim.Target.cs
copy Extras\modules\rust\MulticopterSimEditor_rust.Target.cs Source\MulticopterSimEditor.Target.cs
