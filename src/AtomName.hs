module AtomName where

import RIO (Bool(False), IO, String)
import Graphics.X11 (internAtom, Display)
import Graphics.X11.Xlib (Atom)

newtype AtomName = AtomName String

_NET_WM_STRUT_PARTIAL, _NET_WM_STRUT, _NET_WM_WINDOW_TYPE, _NET_WM_WINDOW_TYPE_DOCK, _ATOM, _CARDINAL :: AtomName
_NET_WM_STRUT_PARTIAL = AtomName "_NET_WM_STRUT_PARTIAL"
_NET_WM_STRUT = AtomName "_NET_WM_STRUT"
_NET_WM_WINDOW_TYPE = AtomName "_NET_WM_WINDOW_TYPE"
_NET_WM_WINDOW_TYPE_DOCK = AtomName "_NET_WM_WINDOW_TYPE_DOCK"
_ATOM = AtomName "ATOM"
_CARDINAL = AtomName "CARDINAL"

mkAtom :: Display -> AtomName -> IO Atom
mkAtom dpy (AtomName name) = internAtom dpy name False
