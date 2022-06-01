#include <stdint.h>
#include <system.h>

#include <debug.h>

// Simulated clock rate
static const uint32_t CLOCK = 2e8;

static double seconds(void)
{
    return FPlatformTime::Seconds(); 
}

extern "C" {

uint32_t systemClockMicrosToCycles(uint32_t micros)
{
    return CLOCK / 1e6 * micros;
}

uint32_t systemGetCycleCounter(void)
{
    static constexpr float PERIOD = 1. / CLOCK;

    static uint32_t counter;

    static double _time;

    double time = seconds();

    if (time - _time > PERIOD) {
        counter++;
        _time = time;
    }

    if (counter == CLOCK) {
        counter = 0;
    }
    
    return counter;
}

uint32_t timeMicros(void)
{
    static double _start;

    _start = _start == 0 ? seconds() : _start;

    return (uint32_t)(1e6 * (seconds() - _start));
}

} // extern "C"
