#@A (C) 1992 Allen I. Holub 

# ----------------------------------------------------------------------
# Makefile to make termlib.lib using Borland C++ and Borland's make.
#
# These routines manipulate the cursor in various ways. The dv_xxx functions
# use direct-video access. They are fast but nonportable. The vb_xxx
# functions use the BIOS. They are slow but portable. Most of the dv_xxx
# functions have vb_xxx equivalents.
# ----------------------------------------------------------------------
# Use the following for compact-model:
#	MODEL = c  for compact (makes termlibc.lib)
#	MODEL = s  for small   (makes termlibs.lib)

MODEL = s
CC = bcc

.c.obj:
	$(CC) -v -c -O -m$(MODEL) { \src\tools\termlib\$*.c }

TARG  = termlib$(MODEL)

# ----------------------------------------------------------------------
OBJ1 = dv_clr_r.obj dv_clrs.obj  dv_print.obj  dv_putc.obj dv_putch.obj
OBJ2 = dv_puts.obj  dv_putsa.obj dv_resto.obj  dv_save.obj dv_scree.obj
OBJ3 = dv_scrol.obj dv_init.obj  dv_frees.obj
OBJ4 = vb_frees.obj vb_getch.obj vb_getyx.obj  vb_iscol.obj
OBJ5 = vb_putc.obj vb_puts.obj  vb_resto.obj   vb_save.obj  _vbios.obj

LIB1 = +dv_clr_r.obj +dv_clrs.obj  +dv_print.obj +dv_putc.obj  +dv_putch.obj
LIB2 = +dv_puts.obj  +dv_putsa.obj +dv_resto.obj +dv_save.obj  +dv_scree.obj
LIB3 = +dv_scrol.obj +dv_init.obj  +dv_frees.obj
LIB4 = +vb_frees.obj +vb_getch.obj +vb_getyx.obj +vb_iscol.obj
LIB5 = +vb_putc.obj  +vb_puts.obj  +vb_resto.obj +vb_save.obj  +_vbios.obj
# ----------------------------------------------------------------------

$(TARG).lib: $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5)
	 rm $(TARG).lib
	 tlib $(TARG).lib /C @&&!
$(LIB1) &
$(LIB2) &
$(LIB3) &
$(LIB4) &
$(LIB5), $(TARG).ndx
!

# ----------------------------------------------------------------------

dv_clr_r.obj:	dv_clr_r.c	video.h
dv_clrs.obj:	dv_clrs.c	video.h
dv_frees.obj:	dv_frees.c	video.h
dv_init.obj:	dv_init.c	video.h
dv_print.obj:	dv_print.c	video.h
dv_putc.obj:	dv_putc.c	video.h
dv_putch.obj:	dv_putch.c	video.h
dv_puts.obj:	dv_puts.c	video.h
dv_putsa.obj:	dv_putsa.c	video.h
dv_resto.obj:	dv_resto.c	video.h
dv_save.obj:	dv_save.c	video.h
dv_scree.obj:	dv_scree.c	video.h
dv_scrol.obj:	dv_scrol.c	video.h

vb_frees.obj:	vb_frees.c	/include/tools/vbios.h
vb_getch.obj:	vb_getch.c	/include/tools/vbios.h
vb_getyx.obj:	vb_getyx.c	/include/tools/vbios.h
vb_iscol.obj:	vb_iscol.c	/include/tools/vbios.h
vb_putc.obj:	vb_putc.c	/include/tools/vbios.h
vb_puts.obj:	vb_puts.c	/include/tools/vbios.h
vb_resto.obj:	vb_resto.c	/include/tools/vbios.h
vb_save.obj:	vb_save.c	/include/tools/vbios.h
_vbios.obj:	_vbios.c	/include/tools/vbios.h video.h
