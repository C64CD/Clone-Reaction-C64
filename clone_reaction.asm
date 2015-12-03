;
; CLONE REACTION
;

; Code and graphics by TMR
; Music by Skywave


; A quick, written-from-scratch copy of the C64 demos W.A.R. by
; Stoat & Tim and Zoolook Moozook by Rob Hubbard - coded for
; C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Memory Map
; $0801 - $17fd		program code/data
; $17fe - $27ff		music
; $2800 - $2fff		character set
; $4000 - $477f		W.A.R. panel workspaces
; $4800 -		scrolling messages and preset data


; Select an output filename
		!to "clone_reaction.prg",cbm


; Pull in the binary data
		* = $17fe
music		!binary "binary\racterslam_loader_2.prg",,2

		* = $2800
		!binary "binary\plain_font_8x8.chr"

		* = $3000
		!binary "binary\sprite_font.spr"


; Raster split positions
raster_1_pos	= $00
raster_2_pos	= $6e
raster_3_pos	= $b6

; Label assignments
raster_num	= $50
sync		= $51

; Labels for the W.A.R. panels
panel_priority	= $52
panel_1_x	= $53
panel_2_x	= $54
panel_pass_ct	= $55
panel_scrl_pos	= $56		; two bytes used

; Labels for the Zoolook Moozook sprites
sprite_0_x	= $58
sprite_0_msb	= $59
sprite_0_y	= $5a

spr_joy		= $5b
spr_joy_temp	= $5c
spr_joy_count	= $5d

ring_count	= $5e

preset_pos	= $60		; two bytes
scroll_pos	= $62		; two bytes
scroll_count	= $64		; two bytes

sprite_x_ring	= $fc00
sprite_msb_ring	= $fd00
sprite_y_ring	= $fe00

; Labels for the Zoolook Moozook background effect
d016_position	= $66
draw_position	= $67

zm_effect_chars	= $2a00


; Add a BASIC startline
		* = $0801
		!word code_start-2
		!byte $40,$00,$9e
		!text "2066"
		!byte $00,$00,$00


; Entry point for the code
		* = $0812

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Initialise some of our own labels
		lda #$01
		sta raster_num

		lda #$00
		sta d016_position
		lda #$40
		sta draw_position

		lda #$50
		sta sprite_0_x
		lda #$01
		sta sprite_0_msb
		lda #$88
		sta sprite_0_y

; Clear the sprite ring buffers
		ldx #$00
		lda #$00
ring_clear	sta sprite_x_ring,x
		sta sprite_msb_ring,x
		sta sprite_y_ring,x
		inx
		bne ring_clear

; Clear the screen
		lda #$00
screen_clear	lda #$00
		sta $0400,x
		sta $0500,x
		sta $0600,x
		sta $06e8,x
		lda #$0a
		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dae8,x
		inx
		bne screen_clear

; Set panel colours
		ldx #$00
panel_colours	lda #$02
		sta $d940,x
		lda #$08
		sta $d968,x
		lda #$07
		sta $d990,x
		lda #$0d
		sta $d9b8,x
		lda #$03
		sta $d9e0,x
		lda #$0e
		sta $da08,x
		lda #$04
		sta $da30,x
		lda #$06
		sta $da58,x
		inx
		cpx #$28
		bne panel_colours

; Set up bordering lines
		ldx #$00
border_set	lda #$3c
		sta $0518,x
		sta $0680,x
		lda #$01
		sta $d918,x
		sta $da80,x
		inx
		cpx #$28
		bne border_set

; Draw in the Zoolook Moozook effect areas
		ldx #$00
zm_effect_init	lda #$40
		sta $0400,x
		sta $06a8,x
		lda #$41
		sta $0428,x
		sta $06d0,x
		lda #$42
		sta $0450,x
		sta $06f8,x
		lda #$43
		sta $0478,x
		sta $0720,x
		lda #$44
		sta $04a0,x
		sta $0748,x
		lda #$45
		sta $04c8,x
		sta $0770,x
		lda #$46
		sta $04f0,x
		sta $0798,x
		inx
		cpx #$27
		bne zm_effect_init


; Set up the music driver
		jsr $2108


; Fetch the first sprite movement command
		jsr preset_reset
		jsr preset_fetch

; Fetch the first block of sprite scroll text
		jsr scroll_reset_2
		jsr spr_scrl_fetch


; Restart the interrupts
		cli


main_start	jsr scroll_reset

; Write text into the first panel
main_init	ldx #$01
mi_loop_1a	jsr scroll_mread
		sta panel_1_ln1+$28,x
		inx
		cpx #$27
		bne mi_loop_1a

		ldx #$01
mi_loop_1b	jsr scroll_mread
		sta panel_1_ln2+$28,x
		inx
		cpx #$27
		bne mi_loop_1b

		ldx #$01
mi_loop_1c	jsr scroll_mread
		sta panel_1_ln3+$28,x
		inx
		cpx #$27
		bne mi_loop_1c

		ldx #$01
mi_loop_1d	jsr scroll_mread
		sta panel_1_ln4+$28,x
		inx
		cpx #$27
		bne mi_loop_1d

		ldx #$01
mi_loop_1e	jsr scroll_mread
		sta panel_1_ln5+$28,x
		inx
		cpx #$27
		bne mi_loop_1e

		ldx #$01
mi_loop_1f	jsr scroll_mread
		sta panel_1_ln6+$28,x
		inx
		cpx #$27
		bne mi_loop_1f

; Write text into the second panel
		ldx #$01
mi_loop_2a	jsr scroll_mread
		sta panel_2_ln1+$28,x
		inx
		cpx #$27
		bne mi_loop_2a

		ldx #$01
mi_loop_2b	jsr scroll_mread
		sta panel_2_ln2+$28,x
		inx
		cpx #$27
		bne mi_loop_2b

		ldx #$01
mi_loop_2c	jsr scroll_mread
		sta panel_2_ln3+$28,x
		inx
		cpx #$27
		bne mi_loop_2c

		ldx #$01
mi_loop_2d	jsr scroll_mread
		sta panel_2_ln4+$28,x
		inx
		cpx #$27
		bne mi_loop_2d

		ldx #$01
mi_loop_2e	jsr scroll_mread
		sta panel_2_ln5+$28,x
		inx
		cpx #$27
		bne mi_loop_2e

		ldx #$01
mi_loop_2f	jsr scroll_mread
		sta panel_2_ln6+$28,x
		inx
		cpx #$27
		bne mi_loop_2f

		lda #$00
		sta panel_priority
		sta panel_1_x
		sta panel_pass_ct
		lda #$50
		sta panel_2_x

; The panel movement routine - waits for the interrupt to set sync to $01
main_loop	jsr sync_wait

; Erase the panel workspace
		ldx #$00
		lda #$00
panel_clear	sta $0540,x
		sta $0568,x
		sta $0590,x
		sta $05b8,x
		sta $05e0,x
		sta $0608,x
		sta $0630,x
		sta $0658,x
		inx
		cpx #$28
		bne panel_clear

; Render the panels
		jsr panel_render

; Update the panel positions
		ldx panel_1_x
		inx
		cpx #$51
		bne px_skip

; The panels are off screen so swap their priorities
		lda panel_priority
		eor #$01
		sta panel_priority

		inc panel_pass_ct

; Pause for a few frames
		ldy #$04
px_wait		jsr sync_wait
		dey
		bne px_wait

		ldx #$00
px_skip		stx panel_1_x

		lda #$50
		sec
		sbc panel_1_x
		sta panel_2_x

; Check to see if we've done enough passes yet - go fetch new panels
; if so
		lda panel_pass_ct
		cmp #$08
		bne main_loop

		jmp main_init


; IRQ interrupt handler
irq_int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne int_go
		jmp irq_exit

; An interrupt has triggered
int_go		lda raster_num
		cmp #$02
		bne *+$05
		jmp irq_rout2

		cmp #$03
		bne *+$05
		jmp irq_rout3


; Raster split 1
; Set the video registers
irq_rout1	lda #$0a
		sta $d020
		sta $d021
		lda #$02
		sta $d022
		lda #$09
		sta $d023

		lda #$1b
		sta $d011

		lda d016_position
		ora #$10
		sta $d016

		lda #$1a
		sta $d018

; Position the hardware sprites
		lda #$ff
		sta $d015

		ldx #$00
set_sprite	lda sprite_pos,x
		sta $d000,x
		inx
		cpx #$11
		bne set_sprite

		ldx #$00
set_sprite_dp	lda sprite_dp,x
		sta $07f8,x
		lda #$00
		sta $d027,x
		inx
		cpx #$08
		bne set_sprite_dp

; Update the Zoolook Moozook effect's scroll position
		ldx d016_position
		inx
		cpx #$08
		bcc *+$04
		ldx #$00
		stx d016_position

; Update the Zoolook Moozook effect itself
		lda draw_position
		sec
		sbc #$01
		and #$7f
		sta draw_position
		tax
		lda #$16
		sta zm_effect_chars,x

		txa
		clc
		adc #$40
		and #$7f
		tax
		lda #$00
		sta zm_effect_chars,x

; Play the music
		jsr $1806

; Set interrupt handler for split 2
		lda #$02
		sta raster_num
		lda #raster_2_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 23
irq_rout2	lda #$0b
		sta $d021
		lda #$00
		sta $d022

		lda #$00
		sta $d016

		lda #$5b
		sta $d011

; Set interrupt handler for split 3
		lda #$03
		sta raster_num
		lda #raster_3_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 3
irq_rout3	lda #$0a
		sta $d021
		lda #$02
		sta $d022

		lda #$1b
		sta $d011

		ldx #$0c
		dex
		bne *-$01

		lda d016_position
		eor #$17
		sta $d016

; Send a message to the runtime code
		lda #$01
		sta sync


; Move the lead sprite
		lda spr_joy
		sta spr_joy_temp

		jsr sprite_0_mover
		jsr sprite_0_mover

; Push the new co-ordinates into the ring buffers
		ldx ring_count
		lda sprite_0_x
		sta sprite_x_ring,x
		lda sprite_0_msb
		sta sprite_msb_ring,x
		lda sprite_0_y
		sta sprite_y_ring,x

; Update the sprite position table for the next frame
		lda sprite_0_x
		sta sprite_pos+$00
		lda sprite_0_msb
		sta sprite_pos+$10
		lda sprite_0_y
		sta sprite_pos+$01

		txa
		sec
		sbc #$1a
		tax
		lda sprite_x_ring,x
		sta sprite_pos+$02
		lda sprite_y_ring,x
		sta sprite_pos+$03
		lda sprite_msb_ring,x
		beq *+$04
		lda #$02
		ora sprite_pos+$10
		sta sprite_pos+$10

		txa
		sec
		sbc #$1a
		tax
		lda sprite_x_ring,x
		sta sprite_pos+$04
		lda sprite_y_ring,x
		sta sprite_pos+$05
		lda sprite_msb_ring,x
		beq *+$04
		lda #$04
		ora sprite_pos+$10
		sta sprite_pos+$10

		txa
		sec
		sbc #$1a
		tax
		lda sprite_x_ring,x
		sta sprite_pos+$06
		lda sprite_y_ring,x
		sta sprite_pos+$07
		lda sprite_msb_ring,x
		beq *+$04
		lda #$08
		ora sprite_pos+$10
		sta sprite_pos+$10

		txa
		sec
		sbc #$1a
		tax
		lda sprite_x_ring,x
		sta sprite_pos+$08
		lda sprite_y_ring,x
		sta sprite_pos+$09
		lda sprite_msb_ring,x
		beq *+$04
		lda #$10
		ora sprite_pos+$10
		sta sprite_pos+$10

		txa
		sec
		sbc #$1a
		tax
		lda sprite_x_ring,x
		sta sprite_pos+$0a
		lda sprite_y_ring,x
		sta sprite_pos+$0b
		lda sprite_msb_ring,x
		beq *+$04
		lda #$20
		ora sprite_pos+$10
		sta sprite_pos+$10

		txa
		sec
		sbc #$1a
		tax
		lda sprite_x_ring,x
		sta sprite_pos+$0c
		lda sprite_y_ring,x
		sta sprite_pos+$0d
		lda sprite_msb_ring,x
		beq *+$04
		lda #$40
		ora sprite_pos+$10
		sta sprite_pos+$10

		txa
		sec
		sbc #$1a
		tax
		lda sprite_x_ring,x
		sta sprite_pos+$0e
		lda sprite_y_ring,x
		sta sprite_pos+$0f
		lda sprite_msb_ring,x
		beq *+$04
		lda #$80
		ora sprite_pos+$10
		sta sprite_pos+$10


		inc ring_count

; Update the preset counter
		dec spr_joy_count
		bne no_new_preset
		jsr preset_fetch

; Update the scrolling message counter
no_new_preset	ldx scroll_count+$00
		dex
		cpx #$ff
		bne *+$04
		dec scroll_count+$01
		stx scroll_count+$00

		cpx #$ff
		bne no_new_scroll
		lda scroll_count+$01
		cmp #$ff
		bne no_new_scroll

		jsr spr_scrl_fetch

no_new_scroll


; Set interrupt handler for split 1
		lda #$01
		sta raster_num
		lda #raster_1_pos
		sta $d012


; Restore registers and exit IRQ interrupt
irq_exit	pla
		tay
		pla
		tax
		pla
nmi_int		rti


; Wait for sync to be set from the interrupt
sync_wait	lda #$00
		sta sync
sw_loop		cmp sync
		beq sw_loop
		rts


; Panel rendering subroutine - calls the actual renderers
panel_render	lda panel_priority
		beq panel_even_top

; Render the panels with the odd-numbered one on top
panel_odd_top	ldy panel_1_x
		jsr panel_render_1
		ldy panel_2_x
		jsr panel_render_2
		rts

; Render the panels with the even-numbered one on top
panel_even_top	ldy panel_1_x
		jsr panel_render_2
		ldy panel_2_x
		jsr panel_render_1
		rts


; Panel renderer 1 (odd-numbered panels)
panel_render_1	ldx #$00
pr1_loop	lda panel_1_ln0,y
		beq *+$05
		sta $0540,x

		lda panel_1_ln1,y
		beq *+$05
		sta $0568,x

		lda panel_1_ln2,y
		beq *+$05
		sta $0590,x

		lda panel_1_ln3,y
		beq *+$05
		sta $05b8,x

		lda panel_1_ln4,y
		beq *+$05
		sta $05e0,x

		lda panel_1_ln5,y
		beq *+$05
		sta $0608,x

		lda panel_1_ln6,y
		beq *+$05
		sta $0630,x

		lda panel_1_ln7,y
		beq *+$05
		sta $0658,x

		iny
		inx
		cpx #$28
		bne pr1_loop
		rts


; Panel renderer 2 (even-numbered panels)
panel_render_2	ldx #$00
pr2_loop	lda panel_2_ln0,y
		beq *+$05
		sta $0540,x

		lda panel_2_ln1,y
		beq *+$05
		sta $0568,x

		lda panel_2_ln2,y
		beq *+$05
		sta $0590,x

		lda panel_2_ln3,y
		beq *+$05
		sta $05b8,x

		lda panel_2_ln4,y
		beq *+$05
		sta $05e0,x

		lda panel_2_ln5,y
		beq *+$05
		sta $0608,x

		lda panel_2_ln6,y
		beq *+$05
		sta $0630,x

		lda panel_2_ln7,y
		beq *+$05
		sta $0658,x

		iny
		inx
		cpx #$28
		bne pr2_loop
		rts


; Read a byte of text from the message
scroll_mread	ldy #$00
		lda (panel_scrl_pos),y
		bne scroll_okay
		jsr scroll_reset
		jmp scroll_mread

scroll_okay	ora #$40

		inc panel_scrl_pos+$00
		bne *+$04
		inc panel_scrl_pos+$01
		rts


; Reset the scrolling messages
scroll_reset	lda #<scroll_text
		sta panel_scrl_pos+$00
		lda #>scroll_text
		sta panel_scrl_pos+$01
		rts

scroll_reset_2	lda #<scroll_text_2
		sta scroll_pos+$00
		lda #>scroll_text_2
		sta scroll_pos+$01
		rts

; Fetch eight bytes of scroll text for sprites
spr_scrl_fetch	ldy #$00
		lda (scroll_pos),y
		bne ssf_okay
		jsr scroll_reset_2
		jmp spr_scrl_fetch

ssf_okay	lda (scroll_pos),y
		clc
		adc #$c0
		sta sprite_dp,y
		iny
		cpy #$08
		bne ssf_okay

		lda scroll_pos+$00
		clc
		adc #$08
		bcc *+$04
		inc scroll_pos+$01
		sta scroll_pos+$00

		lda #$ff
		sta scroll_count+$00
		lda #$01
		sta scroll_count+$01

		rts


; Reset the movement command fetching system
preset_reset	lda #<preset_data
		sta preset_pos+$00
		lda #>preset_data
		sta preset_pos+$01
		rts

; Fetch a new movement command
preset_fetch	ldy #$00
preset_fr	lda (preset_pos),y
		inc preset_pos+$00
		bne *+$04
		inc preset_pos+$01
		cmp #$ff
		bne preset_okay
		jsr preset_reset
		jmp preset_fr

preset_okay	sta spr_joy

		lda (preset_pos),y
		inc preset_pos+$00
		bne *+$04
		inc preset_pos+$01
		sta spr_joy_count
		rts


; Update the lead sprite position based on spr_joy_temp's content
sprite_0_mover

sd_up		lsr spr_joy_temp
		bcc sd_down

		dec sprite_0_y

sd_down		lsr spr_joy_temp
		bcc sd_left

		inc sprite_0_y

sd_left		lsr spr_joy_temp
		bcc sd_right

		ldx sprite_0_x
		dex
		cpx #$ff
		bne sdl_skip
		lda sprite_0_msb
		eor #$01
		sta sprite_0_msb
sdl_skip	stx sprite_0_x

sd_right	lsr spr_joy_temp
		bcc sd_exit

		ldx sprite_0_x
		inx
		bne sdr_skip
		lda sprite_0_msb
		eor #$01
		sta sprite_0_msb
sdr_skip	stx sprite_0_x

sd_exit		rts


; Sprite positions
sprite_pos	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00

sprite_dp	!byte $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7


; Panel 1's work space
		* = $4000
panel_1_ln0	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1b,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1d

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_1_ln1	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_1_ln2	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_1_ln3	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_1_ln4	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_1_ln5	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_1_ln6	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_1_ln7	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $23,$24,$24,$24,$24,$24,$24,$24
		!byte $24,$24,$24,$24,$24,$24,$24,$24
		!byte $24,$24,$24,$24,$24,$24,$24,$24
		!byte $24,$24,$24,$24,$24,$24,$24,$24
		!byte $24,$24,$24,$24,$24,$24,$24,$25

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00


; Panel 2's work space
panel_2_ln0	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1b,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1d

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_2_ln1	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_2_ln2	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_2_ln3	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_2_ln4	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_2_ln5	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_2_ln6	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $1e,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$1f

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

panel_2_ln7	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$3e

		!byte $23,$24,$24,$24,$24,$24,$24,$24
		!byte $24,$24,$24,$24,$24,$24,$24,$24
		!byte $24,$24,$24,$24,$24,$24,$24,$24
		!byte $24,$24,$24,$24,$24,$24,$24,$24
		!byte $24,$24,$24,$24,$24,$24,$24,$25

		!byte $3e,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00


; The message data for the moving panels, arranged in blocks of 228 bytes
; per panel - each is numbered for reference and they're grouped into
; blocks of two for easier reading
scroll_text	!scr "                                     1"
		!scr "  t.m.r presents  a combined tribute  "
		!scr "                                      "
		!scr "  to  zoolook moozook by rob hubbard  "
		!scr "                                      "
		!scr "     and  w.a.r. by stoat and tim     "

		!scr "                                     2"
		!scr "two demo remakes for the price of one!"
		!scr "                                      "
		!scr " and both scrollers are hard to read! "
		!scr "                                      "
		!scr "                                      "


		!scr "as with the original, these plates   3"
		!scr "are eight characters high.            "
		!scr "                                      "
		!scr "so this part of the screen is written "
		!scr "to three times per frame to clear and "
		!scr "then render the two panels.           "

		!scr "the panel code isn't particularly    4"
		!scr "efficient (in part to make it easier  "
		!scr "to read) but that's not an issue      "
		!scr "since everything else doesn't need    "
		!scr "much in the way of processing time.   "
		!scr "                                      "


		!scr "                                     5"
		!scr "  coding and graphics ........ t.m.r  "
		!scr "                                      "
		!scr "  music .................... skywave  "
		!scr "                                      "
		!scr "                                      "

		!scr "code was written with the acme cross 6"
		!scr "assembler.                            "
		!scr "                                      "
		!scr "these characters were created with    "
		!scr "char pad 1.0 and the sprites were put "
		!scr "together using sprite pad 1.8.1       "


		!scr "greetings to all 8-bit fans from the 7"
		!scr "past, present and indeed future.      "
		!scr "                                      "
		!scr "hellos to the harlow cracking service,"
		!scr "rob hubbard, happy democoder and      "
		!scr "stoat and tim.                        "

		!scr "oh, and a quick wave to anyone daft  8"
		!scr "enough to read the c64cd blog, in     "
		!scr "particular those who come back on a   "
		!scr "regular basis!                        "
		!scr "                                      "
		!scr "anti-greetings go out to c64hater...  "


		!scr "there isn't much point in writing    9"
		!scr "vast amounts of text for this demo    "
		!scr "because reading it is really hard...  "
		!scr "                                      "
		!scr "and most people will just look for    "
		!scr "text in memory or the source anyway!  "

		!scr "                                    10"
		!scr "so i might as well finish here.       "
		!scr "                                      "
		!scr "t.m.r, freezing cold and signing off  "
		!scr "in early december 2015... .. .  .     "
		!scr "                                      "

		!byte $00		; end of text marker


; The message for the sprite scroll goes here in chunks of eight characters
; $1c is the heart sprite
scroll_text_2	!scr "clone",$1c,$1c,$1c
		!scr "reaction"
		!scr "by t*m*r"
		!scr "********"

		!scr "music by"
		!scr "skywave*"
		!scr "********"

		!scr "based*on"
		!scr "zoolook",$1c
		!scr "moozook",$1c
		!scr "by*rob*h"
		!scr "and*war",$1c
		!scr "by*stoat"
		!scr "and*tim*"
		!scr "********"
		!byte $1c,$1c,$1c,$1c,$1c,$1c,$1c,$1c
		!scr "********"

		!byte $00		; end of text marker


; These labels are movement commands, here to make reading the following
; table easier!
stop		= $00

up_1		= $01
down_1		= $02
left_1		= $04
right_1		= $08

up_2		= $11
down_2		= $22
left_2		= $44
right_2		= $88

; Spite movement sequence - two bytes per move of direction and duration
; Grouped into individual movement patterns
preset_data	!byte left_1,$f0
		!byte left_1,$f0
		!byte left_1,$f0
		!byte left_1,$f0
		!byte left_1,$f0
		!byte left_1,$70

		!byte left_2,$50
		!byte stop,$40
		!byte left_2,$50
		!byte stop,$40
		!byte left_2,$50
		!byte stop,$40
		!byte left_2,$50
		!byte stop,$40
		!byte left_2,$50
		!byte stop,$40

		!byte up_2+left_1,$28
		!byte down_2+left_1,$50
		!byte up_2+left_1,$28
		!byte up_2+left_1,$28
		!byte down_2+left_1,$50
		!byte up_2+left_1,$28
		!byte up_2+left_1,$28
		!byte down_2+left_1,$50
		!byte up_2+left_1,$28
		!byte up_2+left_1,$28
		!byte down_2+left_1,$50
		!byte up_2+left_1,$28


		!byte right_1,$38
		!byte down_1+right_1,$38
		!byte up_2+right_1,$38
		!byte down_1+right_1,$38
		!byte right_1,$38
		!byte down_1+right_1,$38
		!byte up_2+right_1,$38
		!byte down_1+right_1,$38
		!byte right_1,$38
		!byte down_1+right_1,$38
		!byte up_2+right_1,$38
		!byte down_1+right_1,$38

		!byte down_1+left_1,$50
		!byte left_2,$c0
		!byte up_2+left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte down_2+left_2,$20
		!byte up_1+left_1,$50

		!byte left_2+up_1,$30
		!byte down_1,$30
		!byte left_2+up_1,$30
		!byte down_1,$30
		!byte left_2+up_1,$30
		!byte down_1,$30
		!byte left_2+up_1,$30
		!byte down_1,$30
		!byte left_2+up_1,$30
		!byte down_1,$30
		!byte left_2+up_1,$30
		!byte down_1,$30
		!byte left_2+up_1,$30
		!byte down_1,$30
		!byte left_2+up_1,$30
		!byte down_1,$30
		!byte left_2+up_1,$30
		!byte down_1,$30

		!byte right_2+down_1,$38
		!byte up_1,$38
		!byte right_2+down_1,$38
		!byte up_1,$38
		!byte right_2+down_1,$38
		!byte up_1,$38
		!byte right_2+down_1,$38
		!byte up_1,$38
		!byte right_2+down_1,$38
		!byte up_1,$38
		!byte right_2+down_1,$38
		!byte up_1,$38
		!byte right_2+down_1,$38
		!byte up_1,$38

		!byte down_2+left_2,$26
		!byte up_2+left_2,$26
		!byte up_2+right_1,$26
		!byte down_2+right_1,$26
		!byte down_2+left_2,$26
		!byte up_2+left_2,$26
		!byte up_2+right_1,$26
		!byte down_2+right_1,$26
		!byte down_2+left_2,$26
		!byte up_2+left_2,$26
		!byte up_2+right_1,$26
		!byte down_2+right_1,$26
		!byte down_2+left_2,$26
		!byte up_2+left_2,$26
		!byte up_2+right_1,$26
		!byte down_2+right_1,$26

		!byte up_1+left_1,$50
		!byte left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte left_2,$20
		!byte down_2+left_2,$20
		!byte up_2+left_2,$20
		!byte left_2,$20
		!byte down_1+left_1,$50

		!byte up_1+left_2,$52
		!byte stop,$20
		!byte down_1+left_1,$a4
		!byte stop,$20
		!byte up_1+left_2,$52
		!byte stop,$20
		!byte up_1+left_2,$52
		!byte stop,$20
		!byte down_1+left_1,$a4
		!byte stop,$20
		!byte up_1+left_2,$52
		!byte stop,$20
		!byte up_1+left_2,$52
		!byte stop,$20
		!byte down_1+left_1,$a4
		!byte stop,$20
		!byte up_1+left_2,$52
		!byte stop,$20
		!byte up_1+left_2,$52
		!byte stop,$20
		!byte down_1+left_1,$a4
		!byte stop,$20
		!byte up_1+left_2,$52
		!byte stop,$20

		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28
		!byte up_2+right_2,$28
		!byte down_2,$28

		!byte $ff		; end of data marker
