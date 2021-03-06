/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * IO Manager functionality in the RTS
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

#if defined(mingw32_HOST_OS)

#define IO_MANAGER_WAKEUP 0xffffffff
#define IO_MANAGER_DIE    0xfffffffe
/* spurious wakeups are returned as zero.  */
/* console events are ((event<<1) | 1).  */

int  rts_InstallConsoleEvent ( int action, StgStablePtr *handler );
void rts_ConsoleHandlerDone  ( int ev );
extern StgInt console_handler;

void *   getIOManagerEvent  (void);
HsWord32 readIOManagerEvent (void);
void     sendIOManagerEvent (HsWord32 event);

#else

void     setIOManagerControlFd   (uint32_t cap_no, int fd);
void     setTimerManagerControlFd(int fd);
void     setIOManagerWakeupFd   (int fd);

#endif

/*
 * Communicating with the IO manager thread (see GHC.Conc).
 * Posix implementation in posix/Signals.c
 * Win32 implementation in win32/ThrIOManager.c, Windows's WINIO has the same
 * interfaces for Threaded and Non-threaded I/O, so these methods are always
 * available for WINIO.
*/
void ioManagerWakeup (void);
#if defined(THREADED_RTS) || defined(mingw32_HOST_OS)
void ioManagerDie (void);
void ioManagerStart (void);
#endif
