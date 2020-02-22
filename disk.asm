; Music disk by YQN
; uses perihelion's routines for fading palettes
; gwEm sndh inits

show_vbl	equ	1

scroll_y	equ 200-8
vu_bars_y	equ 112

; track order
num_out		equ	1
num_32		equ	2
num_warm	equ	3
num_bqck	equ 4
num_different	equ	5
num_kqwoo	equ	6
num_journee	equ	7
num_cqrefree	equ 8

	section	text
	
********************** GWEM WANTS MEMORY FOR THE DIGIDRUMS *********************
	
		move.l	4(sp),a5			;address to basepage
		move.l	$0c(a5),d0			;length of text segment
		add.l	$14(a5),d0			;length of data segment
		add.l	$1c(a5),d0			;length of bss segment
		add.l	#$100,d0			;length of basepage
		add.l	#$1000,d0			;length of stackpointer
		move.l	a5,d1				;address to basepage
		add.l	d0,d1				;end of program
		and.l	#-2,d1				;make address even
		move.l	d1,sp				;new stackspace

		move.l	d0,-(sp)			;mshrink()
		move.l	a5,-(sp)			;
		move.w	d0,-(sp)			;
		move.w	#$4a,-(sp)			;
		trap	#1				;
		lea	12(sp),sp			;

*********************** INITS **************************
	
	jsr initialise
	
	lea.l	$ffff8240.w,a0			;white palette
	rept	8
	move.l	#$07770777,(a0)+
	endr
	
	; move.l	#dest_palette,a0
	; add.l	#30,a0
	; move.w	#$777,(a0)
	
*;	BUFFERING
	move.l	#screen1,d0		;put screen1 address in d0
	clr.b	d0				;put on 256 byte boundary	
	move.l	d0,next			;store address
	add.l	#32000,d0		;next screen area
	move.l	d0,last			;store address
*;
	
	move.l	last,a0
	move.l	next,a1
	move.l	#menu_pic+34,a2
	move.l	#7999,d0
.loop_screens
	move.l	(a2),(a0)+
	move.l	(a2)+,(a1)+
	dbra	d0,.loop_screens
	
	; move.l	#menu_pic+2,a2
	; movem.l	(a2),d0-d7
	; movem.l	d0-d7,dest_palette
	

	
	
		; init sndh
		move.l	#out_of_breath,sndh_adr
		jsr		init_sndh
		move.l	#play_sndh,sndh_vbl
		
******************************* steal VBL *******************************

		move.w	sr,d0				;store sr
		move.w	#$2700,sr			;stop all ints
		move.l  $70.w,old_70			; backup $70
		move.l  #main_vbl,$70.w			; ----------------- OCCUPY VBL !!!!!!!!!!!!!!!!!!!!!!!!!!!!
		move.w	d0,sr				;restore sr


*************************************** THE_LOOP ****************************

the_loop
	
	tst.w	menu_active
	beq.s	.no_menu
	; move.w	#7,-(sp)				; wait for keypress
	; trap	#1						; ! this block causes problems when skipping songs automatically
	; addq.l	#2,sp					;
	
	cmp.b	#$4d,$fffffc02.w		; RIGHT ARROW
	bne.s	.not_right
		jsr		erase_cursor
	add.w	#1,tune_number
	jsr		change_tune
	jsr		draw_cursor
.not_right
	
	cmp.b	#$4b,$fffffc02.w		; LEFT
	bne.s	.not_left
		jsr		erase_cursor
	sub.w	#1,tune_number
	jsr		change_tune
	jsr		draw_cursor
.not_left
	
.no_menu
	cmp.b	#$39,$fffffc02.w		;check for space
	bne.s	the_loop

		

		
****************************************** EXITS ******************************
	
		jsr	exit_sndh
		move.l	#dummy,sndh_vbl

	move.l	old_70,$70

	jsr restore
	
	clr.l	-(a7)
	trap	#1					; GEMDOS

	
*******************************************************************************
	

********************************** VVV BBB LLL *******************************
	
main_vbl
	movem.l	d0-d7/a0-a6,-(sp)
	
	move.l	sndh_vbl,a0			;run sndh vbl
	jsr	(a0)

	move.l	next,d0
	clr.b	$ffff820d		;clear STe extra bit	
	lsr.l	#8,d0		
	move.b	d0,$ffff8203	;put in mid screen address byte
	lsr.w	#8,d0
	move.b	d0,$ffff8201	;put in high screen address byte
	
	move.l	last,a0
	move.l	next,a1			; load screens
	move.l	a1,last			; and flip them for next time around
	move.l	a0,next			; double buffering :)
	
	ifne	show_vbl
	move.w	#$505,$ff8240	; vbl time monitoring
	endc
	
	jsr		update_palette
	
	tst.w	scroll_active
	beq.s	.no_scroll
	jsr		scroll
	jsr		feed_scroller
.no_scroll

	tst.w	vu_active
	beq.s	.no_vu
	; jsr		vu_bars
	jsr		vu_2
.no_vu

	tst.w	countdown_active
	beq.s	.no_countdown
	jsr		countdown
.no_countdown

	jsr		poll_sndh

	ifne	show_vbl
	move.w	dest_palette,$ff8240		; restore bg color
	endc
	
	movem.l	(sp)+,d0-d7/a0-a6
	rte
	
	
	
****************************** CLEAR MENU AREA ********************************

clear_menu_area:
	
	rts


	
************************* SCROLL **************************************************	
	
scroll:
	; expects next screen in a1, previous in a0???
	clr.l	d3
	
	ifne	scroll_y
	add.l	#160*scroll_y,a0
	add.l	#160*scroll_y,a1
	endc
	
	add.l	#6,a0				; 4th plane
	add.l	#6,a1

	move.l	#7,d0				; 8 lines
.lines
	move.l	#19,d1				; 20 clusters
.clusters
	move.l	#0,d2				; 4 planes lol
.planes
	move.w	(a0),d3				; get word from prev screen (0)
	lsl.w	d3					; shift 1px
	cmpi.l	#19,d1				; 1st cluster?
	beq		.next
	btst.b	#7,(a0)				; test 1st bit of plane (0)
	beq		.next				; no need to set anything if clear
	bset.b	#0,-7(a1)			; set bit in new screen (-7)
.next
	move.w	d3,(a1)				; copy to new screen (0)
	add.l	#8,a0				; next addresses
	add.l	#8,a1
	dbra	d2,.planes
	dbra	d1,.clusters
	dbra	d0,.lines
	
	rts
	

**************************** SCROLL FEED ************************************
feed_scroller:
	move.l	scrolltext_pointer,a2	; pointer into the message
	clr.l	d0					; clear, just to be sure
	move.b	(a2),d0				; put letter ascii value in d0
	cmpi.b	#0,d0				; end of message?
	bne		.dont_loop_text
	move.l	#scroll_text,scrolltext_pointer	;reset scrolltext_pointer
	bra.s	feed_scroller

.dont_loop_text
	; cmpi.b	#1,d0
	; beq		.pal_1
	; cmpi.b	#2,d0
	; beq		.pal_2
	; cmpi.b	#3,d0
	; beq.s	.pal_3
	; cmpi.b	#4,d0
	; beq.s	.pal_4
	; cmpi.b	#5,d0
	; beq.s	.pal_5
	; bra.w	.feed
	
; .pal_1
	; move.l	#dest_palette,a2
	; add.l	#30,a2
	; move.w	#$750,(a2)
	; add.l	#1,scrolltext_pointer
	; bra.w	feed_scroller
; .pal_2
	; move.l	#dest_palette,a2
	; add.l	#30,a2
	; move.w	#$470,(a2)
	; add.l	#1,scrolltext_pointer
	; bra.w	feed_scroller
; .pal_3
	; move.l	#dest_palette,a2
	; move.l	#menu_pic+2,a3
	; rept	8
	; move.l	(a3)+,(a2)+
	; endr
	; sub.l	#2,a2
	; move.w	#$470,(a2)
	; add.l	#1,scrolltext_pointer
	; bra.w	feed_scroller
; .pal_4
	; move.l	#dest_palette,a2
	; add.l	#30,a2
	; move.w	#$704,(a2)
	; add.l	#1,scrolltext_pointer
	; bra.w	feed_scroller
; .pal_5
	; move.l	#dest_palette,a2
	; add.l	#30,a2
	; move.w	#$770,(a2)
	; add.l	#1,scrolltext_pointer
	; bra.w	feed_scroller
	

.feed
	move.l	#font+34,a2
	subi.l	#$20,d0				; offset so that ' ' is 0
	divu.w	#40,d0				; divide by the # of columns
	clr.l	d1
	move.w	d0,d1				; copy quotient to d1
	tst.l	d1
	beq		.skip				; if quotient is 0: no need to offset vertically
	mulu.w	#160*8,d1
	add.l	d1,a2
	
.skip
	clr.w	d0					; clear quotient only (w)
	swap	d0					; swap with remainder
	divu.w	#2,d0				; div by 2: new quotient = position of 16 px cluster
	bra.s	.check
.loop
	add.l	#8,a2				; offset horizontally
	subi.w	#1,d0
.check
	tst.w	d0
	bne		.loop
	
	swap	d0					; get remainder of d0/2
	move.l	font_counter,d5		; get vertical line in character
	
	move.l	last,a1				; point back to beginning of screen memory

	ifne	scroll_y
	add.l	#160*scroll_y,a0			; y position of the scroll
	add.l	#160*scroll_y,a1
	endc

	add.l	#8*19,a1			; go to last cluster
	add.l	#6,a1				; 6: 4th plane, 4: third...

	move.l	#7,d4
.lines
	move.l	#0,d2				; number of planes
	
.planes
	clr.l	d1
	tst.w	d0
	bne.s		.odd
	move.b	(a2),d1				; get even byte from font (0)
	bra.s	.copy
.odd
	move.b	7(a2),d1			; get odd byte (1)
.copy
	lsr.b	d5,d1				; put bit in last position
	and.b	#%00000001,d1		; keep only this bit
	or.b	d1,1(a1)			; update screen memory (1) (or)
.next
	add.l	#8,a2				; 2 for next plane, 4 for next word, 8 for next longword
	add.l	#8,a1				; depending on # of planes
	dbra	d2,.planes
	
	add.l	#160-8,a2
	add.l	#160-8,a1
	dbra	d4,.lines
	
	tst.l	font_counter
	bne.s	.dont_reset
	move.l	#7,font_counter
	add.l	#1,scrolltext_pointer
	rts
.dont_reset
	sub.l	#1,font_counter
	rts

	
**************************************** VU BARS *****************************

vu_bars:
		clr.l	d0					; will hold volume
		clr.l	d1
		clr.b	d2					; will hold buzzer or noise
		
		move.l	last,a0
		add.l	#160*vu_bars_y+8,a0	; y+x pos
		move.l	next,a1
		add.l	#160*vu_bars_y+8,a1	; y+x pos
		
		move.l	#font+34,a2			; address of font
		add.l	#24*160,a2			; waveforms
		
		move.b	#8,$ff8800			; channel A volume
		move.b	$ff8800,d0			; volume in d0
		move.b	#15,d1
		cmp.b	d0,d1				; test if buzzer
		bpl		.not_buzzer_a
		move.b	#$d,$ff8800			; which waveform
		; move.b	$ff8800,d2
		btst.b	#1,$ff8800			; triangle?
		bne.s	.buzz
		add.l	#16,a2				; address of triangle
		
.buzz
		rept 16
		move.b	(a2),d3
		; move.b	d3,(a0)
		; move.b	d3,2(a0)
		; move.b	d3,4(a0)
		; move.b	d3,6(a0)
		move.b	d3,(a1)
		move.b	d3,2(a1)
		move.b	d3,4(a1)
		move.b	d3,6(a1)
		; or.b	d3,(a0)
		; or.b	d3,2(a0)
		; or.b	d3,4(a0)
		; or.b	d3,6(a0)
		; or.b	d3,(a1)
		; or.b	d3,2(a1)
		; or.b	d3,4(a1)
		; or.b	d3,6(a1)
		add.l	#160,a2
		add.l	#160,a0
		add.l	#160,a1
		endr
		sub.l	#160*16,a0			; restore address in screen
		sub.l	#160*16,a1			; restore address in screen
		move.w	#15,d0				; set level to 15
		
.not_buzzer_a
		; move.b	#7,$ff8800			; noise ?
		; move.b	$ff8800,d2			; noise byte in d2

		move.w	#15,d1				; 16 values
		sub.w	d0,d1				; d1 = # of lines not to display (15-d0)
		bpl		.loop_no_a
		clr.l	d1
.loop_no_a
		; move.b	#$ff,6(a1)
		clr.b	6(a0)
		clr.b	4(a0)
		clr.b	6(a1)
		clr.b	4(a1)
		move.b	#$ff,2(a0)
		move.b	#$ff,(a0)
		add.l	#160,a0				; next line
		move.b	#$ff,2(a1)
		move.b	#$ff,(a1)
		add.l	#160,a1				; next line
		dbra	d1,.loop_no_a
		tst.b	d0					; if level is 0
		beq.s	.channel_b			; next channel
.loop_a
		; btst	#3,d2				; if noise in ch. A
		; beq.s	.not_noise_a
		; and.b	#%10101010,6(a1)	; noise display
		; add.l	#160,a1				; next line
		; dbra	d0,.loop_a
		; bra.s	.channel_b
.not_noise_a
		move.b	#$ff,6(a0)
		add.l	#160,a0				; next line
		move.b	#$ff,6(a1)
		add.l	#160,a1				; next line
		dbra	d0,.loop_a

.channel_b
		clr.l	d0
		clr.l	d1
		clr.l	d2
		
		move.l	next,a1
		add.l	#160*vu_bars_y+16,a1		; y+x pos
		
		move.b	#9,$ff8800			;channel C volume
		move.b	$ff8800,d0			;put volume in d0
		
		move.l	#font+34,a2			; address of font
		add.l	#24*160,a2			; waveforms (bottom)

		move.b	#15,d1
		cmp.b	d0,d1				; test if buzzer
		bpl		.not_buzzer_b
		; or.w	#$070,temp_palette	; show something
		rept 16
		move.b	(a2),d3
		or.b	d3,(a1)
		or.b	d3,2(a1)
		or.b	d3,4(a1)
		or.b	d3,6(a1)
		add.l	#160,a2
		add.l	#160,a1
		endr
		sub.l	#160*16,a1			; restore address in screen
		move.w	#15,d0				; set level to 15
.not_buzzer_b

		move.w	#15,d1				; 16 values
		sub.w	d0,d1				; d1 = # of lines not to display (15-d0)
		bpl		.loop_no_b
		clr.l	d1
.loop_no_b
		clr.b	6(a1)
		clr.b	2(a1)
		move.b	#$ff,4(a1)
		move.b	#$ff,(a1)
		add.l	#160,a1				; next line
		dbra	d1,.loop_no_b
		tst.b	d0
		beq.s	.channel_c
.loop_b
		move.b	#$ff,6(a1)
		add.l	#160,a1				; next line
		dbra	d0,.loop_b

.channel_c
		clr.l	d0
		clr.l	d1
		clr.l	d2
		
		move.l	next,a1
		add.l	#160*vu_bars_y+24,a1		; y+x pos

		move.l	#font+34,a2			; address of font
		add.l	#24*160,a2			; waveforms (bottom)

		move.b	#10,$ff8800			;channel C volume
		move.b	$ff8800,d0			;put volume in d0
		
		move.b	#15,d1
		cmp.b	d0,d1				; test if buzzer
		bpl		.not_buzzer_c
		; or.w	#$007,temp_palette	; show something
		rept 16
		move.b	(a2),d3
		or.b	d3,(a1)
		or.b	d3,2(a1)
		or.b	d3,4(a1)
		or.b	d3,6(a1)
		add.l	#160,a2
		add.l	#160,a1
		endr
		sub.l	#160*16,a1			; restore address in screen
		move.w	#15,d0				; set level to 15
.not_buzzer_c
		move.w	#15,d1				; 16 values
		sub.w	d0,d1				; d1 = # of lines not to display (15-d0)
		bpl		.loop_no_c
		clr.l	d1
.loop_no_c
;		clr.b	6(a1)
		clr.b	(a1)
		move.b	#$ff,2(a1)
		move.b	#$ff,4(a1)
		clr.b	6(a1)
		add.l	#160,a1				; next line
		dbra	d1,.loop_no_c
		tst.b	d0
		beq.s	.vu_done
.loop_c
		move.b	#$ff,6(a1)
		add.l	#160,a1				; next line
		dbra	d0,.loop_c
		
.vu_done
		rts

;-----------------------------		VU 2   ---------------------------
vu_2:
		move.l	next,a1
		move.l	last,a0
		add.l	#160*vu_bars_y+8,a0
		add.l	#160*vu_bars_y+8,a1

		; scroll
		move.l	#2,d1
.clusters
		move.l	#15,d2				; # of lines to scroll -1
.scroll
		move.w	6(a0),d0			; 4th plane
		asl.w	#1,d0				; 1 plan = 1 word
		move.w	d0,6(a1)
		move.w	0(a0),d0			; 4th plane
		asl.w	#1,d0				; 1 plan = 1 word
		move.w	d0,0(a1)
		move.w	2(a0),d0			; 4th plane
		asl.w	#1,d0				; 1 plan = 1 word
		move.w	d0,2(a1)
		move.w	4(a0),d0			; 4th plane
		asl.w	#1,d0				; 1 plan = 1 word
		move.w	d0,4(a1)
		
		add.l	#160,a0
		add.l	#160,a1
		dbra	d2,.scroll
		add.l	#8,a0
		add.l	#8,a1
		sub.l	#160*16,a0			; d2+1 (# of lines) 
		sub.l	#160*16,a1
		dbra	d1,.clusters

		move.l	next,a1				; ----------- reset for CHANNEL A ----------------
		move.l	last,a0
		add.l	#160*vu_bars_y+8,a0
		add.l	#160*vu_bars_y+8,a1
		
		clr.l	d0					; will hold pitch
		clr.l	d1					; will hold volume

		move.b	#0,$ff8800			; channel A pitch
		move.b	$ff8800,d0			; pitch in d0
		move.b	#8,$ff8800			; channel A volume
		move.b	$ff8800,d1			; volume in d1

		divu.w	#16,d0				; get rid of low bits
		and.l	#$0000ffff,d0		; clear remainder
		mulu.w	#160,d0				; apply to position
		add.l	d0,a1
		
		tst.b	d1
		bne.s	.write_a			; if volume not 0 write pixel
		and.w	#$fffe,0(a1)		; else erase pixel
		and.w	#$fffe,2(a1)		; else erase pixel
		and.w	#$fffe,4(a1)		; else erase pixel
		and.w	#$fffe,6(a1)		; else erase pixel
		bra.s	.chan_b
.write_a
		divu.w	#2,d1				; 2 bits resolution for volume (4 colours)
		and.w	#%0000000000000110,d1	; round to low even number and get rid of buzzer bit
		or.w	#1,(a1,d1.w)		; write pixel in the right plane

.chan_b		
		move.l	last,a0				; ----------- reset for CHANNEL B ----------------
		move.l	next,a1

		; add.l	#160*vu_bars_y+8,a0
		add.l	#160*vu_bars_y+8,a1
		; add.L	#8,a0
		add.L	#8,a1
		
		clr.l	d0
		clr.l	d1					; will hold volume

		move.b	#2,$ff8800			; channel B pitch
		move.b	$ff8800,d0			; pitch in d0
		move.b	#9,$ff8800			; channel B volume
		move.b	$ff8800,d1			; volume in d1

		divu.w	#16,d0				; get rid of low nibble
		and.l	#$0000ffff,d0
		mulu.w	#160,d0
		; add.l	d0,a0
		add.l	d0,a1
		
		tst.b	d1
		bne.s	.write_b			; if volume not 0 write pixel
		and.w	#$fffe,0(a1)		; else erase pixel
		and.w	#$fffe,2(a1)		; else erase pixel
		and.w	#$fffe,4(a1)		; else erase pixel
		and.w	#$fffe,6(a1)		; else erase pixel
		bra.s	.chan_c
.write_b
		divu.w	#2,d1				; make it 0-7
		and.w	#%0000000000000110,d1	; round to low even number and get rid of buzzer bit
		or.w	#1,(a1,d1.w)		; write pixel in the right plane

.chan_c		
		move.l	last,a0				; ----------- reset for CHANNEL C ----------------
		move.l	next,a1

		; add.l	#160*vu_bars_y+8,a0
		add.l	#160*vu_bars_y+8,a1
		; add.L	#16,a0
		add.L	#16,a1
		
		clr.l	d0
		clr.l	d1
		
		move.b	#4,$ff8800			; channel C pitch
		move.b	$ff8800,d0			; pitch in d0
		move.b	#10,$ff8800			; channel C volume
		move.b	$ff8800,d1			; volume in d1

		divu.w	#16,d0				; get rid of low nibble
		and.l	#$0000ffff,d0
		mulu.w	#160,d0
		; add.l	d0,a0
		add.l	d0,a1
		
		tst.b	d1
		bne.s	.write				; if volume not 0 write pixel
		and.w	#$fffe,0(a1)		; else erase pixel
		and.w	#$fffe,2(a1)		; else erase pixel
		and.w	#$fffe,4(a1)		; else erase pixel
		and.w	#$fffe,6(a1)		; else erase pixel
		bra.s	.done
.write
		divu.w	#2,d1				; make it 0-7
		and.w	#%0000000000000110,d1	; round to low even number and get rid of buzzer bit
		or.w	#1,(a1,d1.w)		; write pixel in the right plane

.done		
		rts
		
**************************************** PALETTE *************************************

update_palette
		cmp.w	#0,palette_fade_delay
		bne		.not_yet
		move.w	#2,palette_fade_delay
		; move towards dest_palette
		move.l	#dest_palette,a2
		move.l	#temp_palette,a3
		clr.l	d0
		rept	16					; 16 colours
		jsr 	update_red
		jsr 	update_green
		jsr		update_blue
		add.l	#2,a2
		add.l	#2,a3
		endr
		movem.l	temp_palette,d0-d7
		movem.l	d0-d7,$ff8240
.not_yet
		sub.w	#1,palette_fade_delay

		rts


update_red
		move.w  (a2),d0					; move one dest color into d0
		move.w  (a3),d1					; move one temp color into d1

		and.w   #%011100000000,d0		; mask off all but red values
		and.w   #%011100000000,d1		; mask off all but red values

		cmp.w   d1,d0					; see if red is correct intensity
		beq		red_fin
		cmp.w   d1,d0
		bmi     .decrement				; if not ...
		add.w   #%000100000000,(a3)		; ... add one intensity of red
		bra.s	red_fin
.decrement
		sub.w	#%000100000000,(a3)
red_fin
		rts

update_green
		move.w  (a2),d0					; move one dest color into d0
		move.w  (a3),d1					; move one temp color into d1

		and.w   #%000001110000,d0		; mask off all but red values
		and.w   #%000001110000,d1		; mask off all but red values

		cmp.w   d1,d0					; see if red is correct intensity
		beq		green_fin
		cmp.w   d1,d0
		bmi     .decrement				; if not ...
		add.w   #%000000010000,(a3)		; ... add one intensity of red
		bra.s	green_fin
.decrement
		sub.w	#%000000010000,(a3)
green_fin
		rts

update_blue
		move.w  (a2),d0					; move one dest color into d0
		move.w  (a3),d1					; move one temp color into d1

		and.w   #%000000000111,d0		; mask off all but red values
		and.w   #%000000000111,d1		; mask off all but red values

		cmp.w   d1,d0					; see if red is correct intensity
		beq		blue_fin
		cmp.w   d1,d0
		bmi     .decrement				; if not ...
		add.w   #%000000000001,(a3)		; ... add one intensity of red
		bra.s	blue_fin
.decrement
		sub.w	#%000000000001,(a3)
blue_fin
		rts
		
********************************* TEXT ********************************

erase_cursor:
		move.l	last,a0
		move.l	next,a1
		add.l	#160*112+8*8,a0		; cursor position for 1st track -1 (8 pixels above cursor because we're gonna add afterwards)
		add.l	#160*112+8*8,a1
		clr.l	d0
		move.w	tune_number,d0		; track 1 is #1, we're gonna start ofsetting vertically
		mulu.w	#8*160,d0			; amount of memory to skip
		add.l	d0,a0
		add.l	d0,a1
		rept 8
		clr.b	3(a0)
		clr.b	3(a1)
		add.l	#160,a0
		add.l	#160,a1
		endr
		rts
		
draw_cursor:
		move.l	last,a0
		move.l	next,a1
		add.l	#160*112+8*8,a0		; cursor position for 1st track -1 (8 pixels above cursor because we're gonna add afterwards)
		add.l	#160*112+8*8,a1
		clr.l	d0
		move.w	tune_number,d0		; track 1 is #1, we're gonna start ofsetting vertically
		mulu.w	#8*160,d0			; amount of memory to skip
		add.l	d0,a0
		add.l	d0,a1
		
		move.l	#font+34,a2			; get address of '>' sign
		add.l	#15*8,a2
		
		rept 8
		move.b	2(a2),3(a0)
		move.b	2(a2),3(a1)
		add.l	#160,a0
		add.l	#160,a1
		add.l	#160,a2
		endr
		rts

		
print_txt:	; prints null-terminated txt at address in d0
		move.l	d0,a2
		add.l	txt_pointer,a2

		clr.l	d4
		move.b	(a2),d4				; put 1 char in d4
		tst.b	d4					; test if null
		bne.s	.proceed
		move.l	#0,txt_pointer
		bra.w	.done

.proceed		
		move.l	last,a0
		move.l	next,a1
		move.l	#font+34,a2
		clr.l	d2
		move.w	cursor_y,d2
		mulu.w	#160*8,d2			; y pos
		add.l	d2,a0
		add.l	d2,a1
		clr.l	d2
		move.w	cursor_x,d2			; x pos
		divu.w	#2,d2				; get cluster and oddity
		clr.l	d3
		move.w	d2,d3				; move cluster number in d3
		mulu.w	#8,d3
		add.l	d3,a0
		add.l	d3,a1
		
		clr.w	d2
		swap	d2					; get remainder (oddity)

		sub.w	#$20,d4				; ascii
		divu.w	#40,d4
		clr.l	d1
		move.w	d4,d1				; copy quotient to d1
		tst.w	d1
		beq.s	.skip_v
		mulu.w	#160*8,d1			; vertical offset
		add.l	d1,a2
.skip_v
		clr.w	d4
		swap	d4					; get remainder (x pos of 16px cluster)
		divu.w	#2,d4				; # of 16px cluster
		clr.l	d1
		move.w	d4,d1
		sub.w	#1,d1
		bmi.s	.no_h_offset
.loop_h
		add.l	#8,a2
		dbra	d1,.loop_h
		
.no_h_offset
		swap 	d4					; need to know if even or odd char in the font
		
		move.l	#7,d3				; 8 lines
.line
		clr.l	d1
		tst.w	d4
		bne		.odd
		move.b	0(a2),d1			; get even byte from font (0)
		bra.s	.copy
.odd
		move.b	1(a2),d1			; get even byte from font (0)
.copy
		tst.w	d2
		bne.s	.copy_odd
		move.b	d1,(a0)
		move.b	d1,2(a0)
		; move.b	d1,4(a0)
		; move.b	d1,6(a0)
		move.b	d1,(a1)
		move.b	d1,2(a1)
		; move.b	d1,4(a1)
		; move.b	d1,6(a1)
		bra.s	.next
.copy_odd
		move.b	d1,1(a0)
		move.b	d1,3(a0)
		; move.b	d1,5(a0)
		; move.b	d1,7(a0)
		move.b	d1,1(a1)
		move.b	d1,3(a1)
		; move.b	d1,5(a1)
		; move.b	d1,7(a1)
.next
		add.l	#160,a2
		add.l	#160,a1
		add.l	#160,a0
		dbra	d3,.line
		
		add.l	#1,txt_pointer
		add.w	#1,cursor_x
		bra.w	print_txt
.done		
		rts

		
print_year:
	move.l	sndh_adr,a1
	add.l	#16,a1					; tags address
	move.l	#199,d1					; 200 longwords
.loop
	move.l	(a1)+,d2
	; cmp.l	#'COMM',d2
	; bne		.titl
	; move.l	a1,d0
	; bsr		print
	; move.l	#new_line,d0
	; bsr		print
; .titl
	; cmp.l	#"TITL",d2
	; bne		.year
	; move.l	a1,d0
	; bsr		print
	; move.l	#new_line,d0
	; bsr		print
; .year
	cmp.l	#"YEAR",d2
	bne.s	.next
	move.l	a1,d0
	bsr.w	print_txt
	bra.s	.exit_loop
.next	
	add.l	#4,a1
	dbra	d1,.loop
.exit_loop
		rts




************************* SNDH ***********************************
; VBL SNDH-player

init_sndh:
		move.l	sndh_adr,a0			;init sndhfile
		jsr	(a0)				;
		rts
		

play_sndh:
		movem.l	d0-a6,-(sp)			
		move.l	sndh_adr,a0			;sndhfile
		jsr	8(a0)				;play
		movem.l	(sp)+,d0-a6
		rts


exit_sndh:
		move.l	sndh_adr,a0			;deinit sndh
		jsr	4(a0)				;
		rts

		
poll_sndh:
		move.l	sndh_adr,a0
		move.b	$b8(a0),d0			; poll zync byte
		tst.b	d0
		beq.w	.done
		
;		move.w	#$777,temp_palette	; flash

		cmp.b	#1,d0				; zap #1
		bne		.not_zap1
		move.l	#temp_palette,a0
		move.w	#$053,(a0)
		move.w	#$053,6(a0)
		move.w	#$053,10(a0)
		bra.w	.done
.not_zap1
		
		cmp.b	#$f,d0				; if menu (f)
		bne.w	.not_menu
		move.w	#1,menu_active
		move.w	#1,scroll_active
		move.w	#1,vu_active
		
		; move.l	next,a1				; clear screen on 1 plane
		; add.l	#5120,a1
		; move.w	#21760/64-1,d7
; .loop:
		; rept	8				; 16
		; add.l	#4,a1
		; or.l	#$ffff,(a1)+
		; endr
		; dbra	d7,.loop

		jsr		clear_menu_area
		
		lea.l	menu_pic+2,a0
		movem.l	(a0),d0-d7
		movem.l	d0-d7,dest_palette
		bra.w	.done
.not_menu

		cmp.b	#$bd,d0
		bne		.not_bd
		move.l	#temp_palette,a0
		move.w	#$0777,14(a0)
		bra.w	.done
.not_bd

		cmp.b	#$12,d0
		bne.s	.not_21
		move.l	#dest_palette,a0
		move.w	#$0063,16(a0)
		bra.w	.done
.not_21

		cmp.b	#$13,d0
		bne.s	.not_49
		move.l	#dest_palette,a0
		move.w	#$0063,18(a0)
		bra.s	.done
.not_49

		; cmp.b	#$fe,d0					; track looped
		; bne.s	.not_looped
		; add.w	#1,loop_counter
		; cmp.w	#1,loop_counter			; arbitrary value supposed to work for any track whose speed is < 5
		; bne.s	.not_looped
		; move.w	#21,cursor_y
		; move.w	#1,cursor_x
		; move.l	#txt_track_looped,d0
		; jsr		print_txt
		; move.w	#22,cursor_y
		; move.w	#1,cursor_x
		; move.l	#txt_skipping_in,d0
		; jsr		print_txt
		; clr.l	d0						; prevents different flavours from crashing!?!
		; move.w	#1,countdown_active	
; .not_looped		
		
.done
	rts

	
change_tune:
		tst.w	tune_number			; test if 0
		bne.s	.not_zero
		add.w	#1,tune_number		; go back to 1
		bra.w	.done				; don't change track
.not_zero
		cmp.w	#num_cqrefree+1,tune_number
		bne.s	.change				; change tune if we're within the range
		sub.w	#1,tune_number		; otherwise cancel change
		bra.w	.done

.change
		jsr		exit_sndh
		
		cmp.w	#num_out,tune_number
		bne.w	.not_out
		bsr		load_out
		bra.w	.init
.not_out

		cmp.w	#num_32,tune_number
		bne.s	.not_32
		bsr		load_32
		bra.w	.init
.not_32
		
		cmp.w	#num_kqwoo,tune_number
		bne.s	.not_kqwoo
		bsr		load_kqwoo
		bra.w	.init
.not_kqwoo

		cmp.w	#num_bqck,tune_number
		bne.s	.not_bqck
		bsr.w	load_bqck
		bra.s	.init
.not_bqck

		cmp.w	#num_journee,tune_number
		bne.s	.not_journee
		bsr.w	load_journee
		bra.s	.init
.not_journee

		cmp.w	#num_cqrefree,tune_number
		bne.s	.not_cqrefree
		bsr.w	load_cqrefree
		bra.s	.init
.not_cqrefree

		cmp.w	#num_warm,tune_number
		bne.s	.not_warm
		bsr.w	load_warm
		bra.s	.init
.not_warm

		cmp.w	#num_different,tune_number
		bne.s	.not_different
		bsr.w	load_different
;		bra.s	.init
.not_different

		
.init
		move.w	#1,cursor_y
		move.w	#1,cursor_x
		move.l	#txt_year,d0
		jsr		print_txt
		jsr		print_year
		move.l	#txt_3_spcs,d0			; "   ",0
		jsr		print_txt
		move.w	#0,loop_counter			; reset loop counter
		move.w	#250,vbl_countdown		; reset countdown
		move.w	#0,countdown_active		; deactivate
		; clr.l	d0						; for some reason this line sometimes prevents from crashing during warm ember
		jsr		draw_cursor
		jsr		init_sndh
		move.l	#play_sndh,sndh_vbl
.done
		rts
	


	
load_kqwoo:
;		jsr		exit_sndh
		move.l	#kqwooth,sndh_adr
		
		move.l	last,a0
		move.l	next,a1
		move.l	#bg_kqwooth+34,a2
		move.l	#3839,d0
.loop
		move.l	(a2),(a0)+
		move.l	(a2)+,(a1)+
		dbra	d0,.loop
		
		; move.l	#bg_kqwooth+2,a2
		; movem.l	(a2),d0-d7
		; movem.l	d0-d7,dest_palette
		rts
	
load_warm:
;		jsr		exit_sndh
		move.l	#warm_ember,sndh_adr
		
		move.l	last,a0
		move.l	next,a1
		move.l	#bg_kqwooth+34+96*160,a2
		move.l	#3839,d0
.loop
		move.l	(a2),(a0)+
		move.l	(a2)+,(a1)+
		dbra	d0,.loop
		
		; movem.l	warm_palette,d0-d7
		; movem.l	d0-d7,dest_palette
		rts

load_out:
;		jsr		exit_sndh
		move.l	#out_of_breath,sndh_adr
		
		move.l	#menu_pic+2,a2				; palette
		movem.l	(a2),d0-d7
		movem.l	d0-d7,dest_palette
		
		add.l	#32,a2						; image
		move.l	last,a0
		move.l	next,a1
		move.l	#3839,d0					; 96 lines
.loop
		move.l	(a2),(a0)+
		move.l	(a2)+,(a1)+
		dbra	d0,.loop

		rts
		
load_32:
;		jsr		exit_sndh
		move.l	#s_32,sndh_adr
		
		move.l	#bg_32+2,a2					; palette
		movem.l	(a2),d0-d7
		movem.l	d0-d7,dest_palette
		
		add.l	#32,a2						; image
		move.l	last,a0
		move.l	next,a1
		move.l	#3839,d0					; 96 lines
.loop
		move.l	(a2),(a0)+
		move.l	(a2)+,(a1)+
		dbra	d0,.loop
		rts
	
load_different:
;		jsr		exit_sndh
		move.l	#different_flqvours,sndh_adr
		
		rts
	
load_bqck:
;		jsr		exit_sndh
		move.l	#bqckinb,sndh_adr
		
		rts
	
load_cqrefree:
;		jsr		exit_sndh
		move.l	#cqrefree,sndh_adr
		
		rts
	
load_journee:
;		jsr		exit_sndh
		move.l	#journee,sndh_adr
		
		rts


countdown:
		sub.w	#1,vbl_countdown
		tst.w	vbl_countdown
		bne.s	.update_display
		jsr		erase_cursor
		add.w	#1,tune_number				; next tune
		bsr.w	change_tune
		bra.w	.done
.update_display
		cmp.w	#200,vbl_countdown			; 4 secs left
		bne.s	.not_4
		move.w	#13,cursor_x
		move.w	#22,cursor_y
		move.l	#txt_4,d0
		jsr		print_txt		
.not_4
		cmp.w	#150,vbl_countdown			; 3 secs left
		bne.s	.not_3
		move.w	#13,cursor_x
		move.w	#22,cursor_y
		move.l	#txt_3,d0
		jsr		print_txt
.not_3
		cmp.w	#100,vbl_countdown			; 2 secs left
		bne.s	.not_2
		move.w	#13,cursor_x
		move.w	#22,cursor_y
		move.l	#txt_2,d0
		jsr		print_txt
.not_2
		cmp.w	#50,vbl_countdown			; 1 sec left
		bne.s	.not_1
		move.w	#13,cursor_x
		move.w	#22,cursor_y
		move.l	#txt_1,d0
		jsr		print_txt
.not_1
		
.done
		rts
	
dummy:
		rts




******************************************************************************
	
	include	'initlib2.s'
;	include	'msg.asm'

*******************************************************************************

	section	data

draw_vbl		dc.w	0
menu_active		dc.w	0
scroll_active	dc.w	0
vu_active		dc.w	0
old_70			dc.l	0
loop_counter	dc.w	0
vbl_countdown	dc.w	250
countdown_active	dc.w	0
tune_number:	dc.w	1

palette_fade_delay:	dc.w	2
	even
sndh_vbl:
		dc.l	dummy

		
out_of_breath:	incbin	'sndh\breqth5z.SND'			;must be UNPACKED!
		even
warm_ember:	incbin	'sndh\WARMEMB7.SND'			;must be UNPACKED!
		even
s_32:	incbin	'sndh\329.SND'			;must be UNPACKED!
		even
cqrefree:	incbin	'sndh\CQREFR14.SND'
		even
kqwooth:	incbin	'sndh\kqwoothz.SND'
		even
bqckinb:	incbin	'sndh\BQCKINB4.SND'
		even
different_flqvours:	incbin	'sndh/differen.snd'
		even
journee:	incbin	'sndh\journee.snd'
		even



font
	incbin	'0182_.PI1'
	even
menu_pic
	incbin	'ym_menu3.pi1'
	even
bg_kqwooth
		incbin	'bg_kqwoo.pi1'
	even
bg_32
		incbin	'bg_32.pi1'
	even
	
	
next	dc.l	0
last	dc.l	0

font_counter
	dc.l	7		; (character width / speed) - 1

scroll_text
	dc.b	"    Welcome to my very first assembly project and most importantly my first music disk!"
	dc.b	"                It uses pieces of code by perihelion, gwEm and dhs. "
	dc.b	"Before I start writing boring stuff in this scroller let me thank those who "
	dc.b	"helped making this disk possible: "
	dc.b	"gwEm for his stunning tracker -*- akuyou for putting me back on track and for his AweSoMe development kit -*- "
	dc.b	"MarkeyJester for his fantastic lessons -*- nguillaumin for the html versions of perihelion's tuts -*- "
	dc.b	"everybody who answered my n00b questions @ atari-forum.com: "
	dc.b	"AtariZoll, Cyprian, dhedberg, dma-sc, ggn, Grazey, Greenious, mikro, Mug UK, robdaemon, simonsunnyboy, Stethane, "
	dc.b	"thomas3, wietze, Zorro 2. Apologies if I've forgotten anyone, what are the odds that they're reading this anyway? "
	dc.b	"Now the likelihood that perihelion is reading this is sadly even lower indeed, yet he probably was the most helpful. "
	dc.b	"This humble disk is dedicated to his memory. "
	dc.b	"                                        ",0	; 40 spaces: 1 screen width
	even
scrolltext_pointer	dc.l	scroll_text

cursor_x	dc.w	0
cursor_y	dc.w	0

txt_year	dc.b	"Year: ",0
	even
txt_3_spcs	dc.b	"   ",0
	even
txt_track_looped	dc.b	"Track looped,",0
	even
txt_skipping_in		dc.b	"skipping in  s",0
	even
txt_4		dc.b	"4",0
	even
txt_3		dc.b	"3",0
	even
txt_2		dc.b	"2",0
	even
txt_1		dc.b	"1",0
	even

txt_pointer	dc.l	0

font_even_char	dc.w	0	; 0 = even, 1 = odd
font_char_addr	dc.l	0


warm_palette:
	; dc.w	$001,$102,$650,$542,$650,$542,$026,$026
	; dc.w	$770,$770,$430,$540,$540,$640,$627,$777
	dc.w	$112,$000,$231,$451,$192,$122,$024,$047
	dc.w	$521,$634,$741,$763,$444,$577,$777,$070
kqwooth_palette:
	dc.w	$775,$067,$056,$467,$650,$467,$467,$775
	dc.w	$770,$775,$430,$540,$540,$640,$627,$012
	
prev_pitch_A:	dc.w	160
prev_pitch_B:	dc.w	160
prev_pitch_C:	dc.w	160

; debug
prev_l	dc.l	0
	even

***********************************************************************************

	section bss
	
	ds.b	256
screen1	ds.b	32000
screen2	ds.b	32000
	even

dest_palette	ds.w	16
temp_palette	ds.w	16

sndh_adr:	ds.l	1				;address to SNDH-file

