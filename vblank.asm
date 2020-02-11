SECTION "VBlank", ROM0[$0060]

VBlank:: ; $0060
; Coincides with the joypad interrupt, which is unused afaict
	push af
	push bc
	push de
	push hl
	call DrawColumn		; Drawing new areas of the map
	call BlockCollision		; Collision with coins, coin blocks, etc...?
	call UpdateLives
	call hDMARoutine
	call DisplayScore
	call DisplayTimer
	call AnimateBackground
	ld hl, hFrameCounter
	inc [hl]
	ldh a, [hGameState]
	cp a, STATE_GAMEOVER	; Game over?
	jr nz, .gameNotOver
	ld hl, rLCDC
	set 5, [hl]			; Turn on window
.gameNotOver
	xor a
	ldh [rSCX], a
	ldh [rSCY], a
	inc a
	ldh [hVBlankOccurred], a
	pop hl
	pop de
	pop bc
	pop af
	reti