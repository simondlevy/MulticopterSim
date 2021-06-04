::
:: Socket module setup script for MulticopterSim
::
:: Copyright (C) 2021 Simon D. Levy
::
:: MIT License

clean.bat

copy Extras\modules\MulticopterSim_startup.uproject MulticopterSim.uproject
copy Extras\modules\MulticopterSim_startup.Target.cs Source\MulticopterSim.Target.cs
copy Extras\modules\MulticopterSimEditor_startup.Target.cs Source\MulticopterSimEditor.Target.cs
