SECTION "LCD Stat", ROM0[$0095]

; Update scroll registers.
; Called after the 16th scanline, so the HUD doesn't scroll. Also called later
; to turn the window off
LCDStatus::
	push af
	push hl
	WAIT_FOR_HBLANK
	ld a, [wGameOverWindowEnabled]
	and a
	jr nz, .turnOffWindow
	ldh a, [hScrollX]
	ldh [rSCX], a
	ld a, [$C0DE]
	and a
	jr z, .checkForGameOver
	ld a, [wScrollY]
	ldh [rSCY], a
.checkForGameOver
	ldh a, [hGameState]
	cp a, STATE_GAMEOVER
	jr nz, .out			; game not over
	ld hl, rWY
	ld a, [hl]
	cp a, $40
	jr z, .countdownGameOverTimer
	dec [hl]			; scroll the window with game over text up
	cp a, $87			; Until the bottom is visible, don't bother switching
	jr nc, .out			; it off
.scheduleInterrupt
	add a, 8			; schedule an interrupt in 8 scanlines to turn
	ldh [rLYC], a		; the window off
	ld [wGameOverWindowEnabled], a
.out
	pop hl
	pop af
	reti

.turnOffWindow
	ld hl, rLCDC
	res 5, [hl]		; Turn off Window
	ld a, $0F		; height of the HUD
	ldh [rLYC], a
	xor a
	ld [wGameOverWindowEnabled], a
	jr .out

.countdownGameOverTimer
	push af
	ldh a, [$FFFB]
	and a
	jr z, .timerExpired
	dec a
	ldh [$FFFB], a
.outTimer
	pop af
	jr .scheduleInterrupt

.timerExpired
	ld a, $FF
	ld [wGameOverTimerExpired], a
	jr .outTimer