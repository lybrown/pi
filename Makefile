run: pi.xex
	altirra $<
pi.xex: pi.s pi.cfg pimacros.m
	ca65 pi.s -o pi.o
	ld65 -o pi.xex -C pi.cfg pi.o
