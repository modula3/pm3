#ifndef __CONFIG_H
#define __CONFIG_H

#ifdef __GNUC__
#define NORETURN(f) f __attribute__ ((noreturn))
#else
#define NORETURN(f) f
#endif

#endif
