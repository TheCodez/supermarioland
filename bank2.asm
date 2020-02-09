INCLUDE "charmap.asm"
INCLUDE "gbhw.asm"
INCLUDE "hram.asm"
INCLUDE "vram.asm"

; todo replace with level data
SECTION "bank 2", ROMX, BANK[2]
LevelPointersBank2:: ; 2:4000
	dw $6192	; 1-1
	dw $61B7	; 1-2
	dw $61DA	; 1-3
	dw $6192
	dw $61B7
	dw $61DA
	dw $6192
	dw $61B7
	dw $61DA
	dw $6192
	dw $61B7
	dw $61DA
	dw MenuLevelData	; Start Menu

LevelEnemyPointersBank2:: ; 2:401A
	dw $6002	; 1-1
	dw $6073	; 1-2
	dw $60FE	; 1-3
	dw $6002
	dw $6073
	dw $60FE
	dw $6002
	dw $6073
	dw $60FE
	dw $6002
	dw $6073
	dw $60FE

CommonTiles1::
INCBIN "gfx/commonTiles1.2bpp"
CommonTiles1End:

EnemiesWorld1::
INCBIN "gfx/enemiesWorld1.2bpp"
EnemiesWorld1End:

CommonTiles2::
INCBIN "gfx/commonTiles2.2bpp"
CommonTiles2End:

BackgroundWorld1::
INCBIN "gfx/backgroundWorld1.2bpp"
BackgroundWorld1End:

CommonTiles3::
INCBIN "gfx/commonTiles3.2bpp"
CommonTiles3End:

; 5832
; Bonus game GameState routines
; No idea why this level of indirection
HandleBonusGameMarioSprites:: ; 5832
	jp _HandleBonusGameMarioSprites

HandleBonusGame:: ; 5835
	jp _GameState_15

GameState_17:: ; 5838
	jp _GameState_17

GameState_18:: ; 583B
	jp _GameState_18

GameState_19:: ; 583E
	jp _GameState_19

GameState_1A:: ; 5841
	jp _GameState_1A

; why
UpdateTimerAndFloaties:: ; 5844
	call UpdateGameTimer
	call UpdateFloaties
	ret

; Make the Timer count down. The logic in this routine is unnecessarily
; convoluted. Bug
UpdateGameTimer:: ; 584B
	ld a, [wGameTimerExpiringFlag]
	cp a, 3
	ret z
	ld hl, wGameTimer
	ld a, [hl]
	dec a				; decrement the unseen "hundredths" counter
	ld [hl], a
	ret nz
	ld a, $28			; 40 frames per tick, 2/3rd second. Why not 60?
	ld [hl], a
	inc hl
	ldi a, [hl]			; ones and tens
	ld c, [hl]			; hundreds
	dec hl
	sub a, 1
	daa
	ldi [hl], a
	cp a, $99
	jr nz, .checkTimerExpiration
	dec c				; decrement one from the hundreds
	ld a, c
	ld [hl], a
	ret

.checkTimerExpiration	; called almost every frame. Ridiculous, bug
	ld hl, wGameTimerExpiringFlag
	cp a, $50
	jr z, .checkIfUnder50
	and a
	ret nz
	or c
	jr nz, .checkIfUnder100
	ld a, 3
	ld [hl], a
	ret

.checkIfUnder50
	ld a, c
	and a
	ret nz
	ld a, 2
	ld [hl], a
	ld a, $50
	ldh [rTMA], a		; speed up timer, speeds up music
	ret

.checkIfUnder100
	ld a, c
	cp a, $01
	ret nz
	ld a, 1
	ld [hl], a
	ld a, $30
	ldh [rTMA], a		; speed up timer
	ret

UpdateFloaties:: ; 5892
	ldh a, [hFloatyControl]
	ld b, a
	and a					; if non zero, spawn a new floaty
	jp z, .updateFloaties
	ld a, [wNextFloatyOAMIndex]
	ld l, a
	ld h, HIGH(wOAMBuffer)
	ld de, $8				; two objects per floaty
	push hl
	add hl, de
	ld a, l
	ld [wNextFloatyOAMIndex], a
	cp a, $50				; Four slots are used: 30 - 38 - 40 - 48
	jr nz, .initFloatyParameters
	ld a, $30
	ld [wNextFloatyOAMIndex], a
.initFloatyParameters
	pop hl
	ld c, $20				; default floaty TTL is 20 times 2 frames
	ld d, $F6				; only used for the coin?
	ld a, l
	cp a, $30
	jr nz, .checkFloaty1
	ld a, c
	ld [wFloaty0_TTL], a
	ld a, d
	ld [wFloaty0_SpriteIfCoin], a
	ld a, b
	cp a, $C0
	jr nz, .setupCoordinates
	ld [wFloaty0_IsCoin], a
	jr .setupCoordinates

.checkFloaty1
	cp a, $38
	jr nz, .checkFloaty2
	ld a, c
	ld [wFloaty1_TTL], a
	ld a, d
	ld [wFloaty1_SpriteIfCoin], a
	ld a, b
	cp a, $C0
	jr nz, .setupCoordinates
	ld [wFloaty1_IsCoin], a
	jr .setupCoordinates

.checkFloaty2
	cp a, $40
	jr nz, .checkFloaty3
	ld a, c
	ld [wFloaty2_TTL], a
	ld a, d
	ld [wFloaty2_SpriteIfCoin], a
	ld a, b
	cp a, $C0
	jr nz, .setupCoordinates
	ld [wFloaty2_IsCoin], a
	jr .setupCoordinates

.checkFloaty3
	ld a, c
	ld [wFloaty3_TTL], a
	ld a, d
	ld [wFloaty3_SpriteIfCoin], a
	ld a, b
	cp a, $C0
	jr nz, .setupCoordinates
	ld [wFloaty3_IsCoin], a
.setupCoordinates
	ldh a, [hFloatyY]
	ldi [hl], a			; OAM Y
	ldh a, [hFloatyX]
	ldi [hl], a			; OAM X
	ld a, b
	ld de, $5958		;  1 00
	cp a, $01
	jr z, .setupTiles
	inc d			; 6058  2 00
	cp a, $02
	jr z, .setupTiles
	inc d			; 6158  4 00
	cp a, $04
	jr z, .setupTiles
	inc d			; 6258  5 00
	cp a, $05
	jr z, .setupTiles
	inc d			; 6358  8 00
	cp a, $08
	jr z, .setupTiles
	ld d, $59
	dec e			; 5957 10 00
	cp a, $10
	jr z, .setupTiles
	inc d			; 5A57 20 00
	cp a, $20
	jr z, .setupTiles
	inc d			; 5B57 40 00
	cp a, $40
	jr z, .setupTiles
	inc d			; 5C57 50 00
	cp a, $50
	jr z, .setupTiles
	inc d			; 5D57 80 00
	cp a, $80
	jr z, .setupTiles
	inc d			; 5E57 80 00
	ld e, $5F		; 5E5F  1UP
	cp a, $FF
	jr z, .setupTiles
	ld de, $F6FE	; F6FE coin sprite
.setupTiles
	ld a, d			; left object
	ldi [hl], a
	inc hl
	ldh a, [hFloatyY]
	ldi [hl], a
	ldh a, [hFloatyX]
	add a, $8		; one tile to the right
	ldi [hl], a
	ld a, e			; right object
	ld [hl], a
	xor a
	ldh [hFloatyControl], a
	ldh [hFloatyY], a
	ldh [hFloatyX], a
	ld a, b			; floaty control
	ld de, $0100	;  100
	cp a, $01
	jr z, .addFloatyScore
	inc d			;  200
	cp a, $02
	jr z, .addFloatyScore
	inc d
	inc d			;  400
	cp a, $04
	jr z, .addFloatyScore
	inc d			;  500
	cp a, $05
	jr z, .addFloatyScore
	ld d, $08		;  800
	cp a, $08
	jr z, .addFloatyScore
	ld d, $10		; 1000
	cp a, $10
	jr z, .addFloatyScore
	ld d, $20		; 2000
	cp a, $20
	jr z, .addFloatyScore
	ld d, $40		; 4000
	cp a, $40
	jr z, .addFloatyScore
	ld d, $50		; 5000
	cp a, $50
	jr z, .addFloatyScore
	ld d, $80		; 8000
	cp a, $80
	jr z, .addFloatyScore
	jr .updateFloaties

.addFloatyScore
	call AddScore
.updateFloaties
	ld hl, wOAMBuffer + $30
.updateFloaty
	push hl
	ld a, [hl]
	and a
	jp z, .nextFloaty
	ld a, l
	ld bc, wFloaty3_TTL
	ld de, wFloaty3_SpriteIfCoin
	ld hl, $DA13
	cp a, $48
	jr z, .moveFloaty3
	dec c
	dec e
	dec l
	cp a, $40
	jr z, .moveFloaty2
	dec c
	dec e
	dec l
	cp a, $38
	jr z, .moveFloaty1
	dec c
	dec e
	dec l
.moveFloaty0			; unused jump
	ld a, [wFloaty0_IsCoin]
	cp a, $C0
	jr z, .moveFloaty
	ld a, [hl]
	inc a
	ld [hl], a
	cp a, $02
	jp nz, .nextFloaty
	xor a
	ld [hl], a
	jr .moveFloaty

.moveFloaty1
	ld a, [wFloaty1_IsCoin]
	cp a, $C0
	jr z, .moveFloaty
	ld a, [hl]
	inc a
	ld [hl], a
	cp a, $02
	jp nz, .nextFloaty
	xor a
	ld [hl], a
	jr .moveFloaty

.moveFloaty2
	ld a, [wFloaty2_IsCoin]
	cp a, $C0
	jr z, .moveFloaty
	ld a, [hl]
	inc a
	ld [hl], a
	cp a, $02
	jr nz, .nextFloaty
	xor a
	ld [hl], a
	jr .moveFloaty

.moveFloaty3
	ld a, [wFloaty3_IsCoin]
	cp a, $C0
	jr z, .moveFloaty
	ld a, [hl]
	inc a
	ld [hl], a
	cp a, $02
	jr nz, .nextFloaty
	xor a
	ld [hl], a
.moveFloaty
	pop hl
	push hl
	dec [hl]		; Y pos, one pixel up
	inc l
	inc l
	inc l
	inc l
	dec [hl]		; move up second sprite
	dec l
	dec l
	ld a, [hl]		; todo unentangle this logic
	cp a, $F6		; first coin sprite
	jr c, .decrementTTL
	ld a, [de]		; wFloatyN_SpriteIfCoin
	inc a
	ld [de], a
	ld [hl], a
	cp a, $F9		; one more than the last coin sprite
	jr c, .decrementTTL
	dec a
	dec a
	ld [hl], a
	cp a, $F7
	jr z, .decrementTTL
	dec a
	dec a
	ld [de], a
	ld [hl], a
.decrementTTL
	ld a, [bc]		; wFloatyN_TTL
	dec a
	ld [bc], a
	jr nz, .nextFloaty
	ld a, $20		; reset floaty
	ld [bc], a
	ld a, $F6
	ld [de], a
	xor a			; hide objects
	ldd [hl], a		; object tile
	ldd [hl], a		; object X pos
	ldi [hl], a		; object Y pos
	inc l
	inc l
	inc l
	ldi [hl], a		; Y pos
	ldi [hl], a		; X pos
	ld [hl], a		; tile number
	ld a, l
	ld hl, wFloaty0_IsCoin
	ld bc, $0004
	cp a, $36
	jr z, .resetCoinStatus
	inc l
	cp a, $3E
	jr z, .resetCoinStatus
	inc l
	cp a, $46
	jr z, .resetCoinStatus
	inc l
.resetCoinStatus
	xor a
	ld [hl], a		; wFloatyN_IsCoin
	add hl, bc
	ld [hl], a		; wFloatyN_?
.nextFloaty
	pop hl
	ld de, $0008
	add hl, de
	ld a, l
	cp a, $50
	jp nz, .updateFloaty
	ret

; setup Mario's sprite in OAM
_HandleBonusGameMarioSprites:: ; 5A72
	ld hl, wOAMBuffer + 4 * $C
	ldh a, [rDIV]
	and a, $3
	inc a
	ld b, a
	ld a, $20
.loop
	add a, $18
	dec b
	jr nz, .loop		; A = $20 + $18 * rand(1,4)
	ld b, a
	ldi [hl], a		; Y pos
	ld a, $10
	ld c, a
	ldi [hl], a		; X pos
	xor a
	ld d, a
	ldh a, [hSuperStatus]
	cp a, 2
	jr nz, .jmp_5A93
	ld a, $20
	ld d, a
.jmp_5A93
	ld a, d
	ldi [hl], a		; tile number
	inc l			; no attributes
	ld a, b
	ldi [hl], a		; Y pos
	ld a, c
	add a, 8		; one to the right
	ldi [hl], a		; X pos
	ld a, d
	inc a
	ldi [hl], a		; tile number
	inc l			; no attributes
	ld a, b
	add a, 8		; one down
	ld b, a
	ldi [hl], a		; Y pos
	ld a, c
	ldi [hl], a		; X pos
	ld a, d
	add a, 16
	ld d, a
	ldi [hl], a		; tile number
	inc l			; no attributes
	ld a, b
	ldi [hl], a		; Y pos
	ld a, c
	add a, 8		; one to the right
	ldi [hl], a		; X pos
	inc d
	ld a, d
	ld [hl], a		; tile number
	ld a, $15
	ldh [hGameState], a
	ret

_GameState_15:: ; 5ABB
	ld a, [$DA27]	; ladder status?
	bit 0, a
	jr z, .jmp_5AC9
	ldh a, [hJoyHeld]
	bit 0, a		; A button todo
	jp nz, .jmp_5B56
.jmp_5AC9
	ld hl, wBonusGameFrameCounter
	ld a, [hl]
	inc a
	ld [hl], a
	cp a, 3			; animate every 3 frames
	ret nz
	xor a
	ld [hl], a
	ld a, [$DA27]
	bit 0, a
	jr z, .jmp_5B07
	ld hl, wOAMBuffer + 4 * $C
	ld b, 4
	ld a, [hl]		; Y pos
	cp a, $80		; bottom floor
	jr z, .jmp_5AF1
.loop1
	ld a, $18
	add [hl]		; down 3 tiles
	ldi [hl], a
	inc l
	inc l
	inc l
	dec b
	jr nz, .loop1
	jr .jmp_5B07

.jmp_5AF1
	ld b, 2			; 2 bottom objects
	ld a, $38		; top floor
.loop2
	ldi [hl], a
	inc l
	inc l
	inc l
	dec b
	jr nz, .loop2
	ld b, 2			; 2 top objects
	ld a, $40
.loop3
	ldi [hl], a
	inc l
	inc l
	inc l
	dec b
	jr nz, .loop3
.jmp_5B07
	ld hl, vBGMap0 + $EA ; $98EA	; ladder top floor position
	ld bc, $0060	; 3 screen widths
	ld de, $DA27
	ld a, [de]
	inc a
	ld [de], a
	cp a, $03
	jr c, .jmp_5B27
	add hl, bc
	cp a, $05
	jr c, .jmp_5B27
	add hl, bc
	cp a, $07
	jr c, .jmp_5B27
	ld hl, $98EA
	xor a
	inc a
	ld [de], a
.jmp_5B27
	ld a, h
	ld [wLadderLocationHi], a
	ld a, l
	ld [wLadderLocationLo], a
	ld hl, wLadderTiles
	ld a, [de]
	bit 0, a
	jr z, .jmp_5B45
	ld a, $2E
	ldi [hl], a
	ld a, $2F
	ldi [hl], a
	ld a, $2F
	ldi [hl], a
	ld a, $30
	ld [hl], a
	jr .jmp_5B51

.jmp_5B45
	ld a, $2D
	ldi [hl], a
	ld a, $2C
	ldi [hl], a
	ld a, $2C
	ldi [hl], a
	ld a, $2D
	ld [hl], a
.jmp_5B51
	ld a, $16
	ldh [hGameState], a
	ret

.jmp_5B56
	xor a
	ld [$DA22], a
	ld [$DA27], a
	ld [$DA1A], a
	ld a, $17
	ldh [hGameState], a
	ret

_GameState_17:: ; 5B65
	ld hl, $DA1C			; 1 if walking
	ld a, [hl]
	and a
	jr nz, .jmp_5B73
	inc [hl]				; start walking
	ld hl, $DFE8
	ld a, $0A				; walking music
	ld [hl], a
.jmp_5B73
	ld hl, wOAMBuffer + 4 * $C + 1	; X pos
	ld de, $5C9D			; todo
	ld b, $04
	ld a, [$DA14]			; animation index
	and a
	jr z, .objectLoop		; Tssk
.loop
	inc de
	dec a
	jr nz, .loop			; "ADD DE, A"
.objectLoop
	inc [hl]				; move one pixel forwards
	inc l
	ld a, [de]
	ld c, a
	cp a, $FF				; end of animation, restart
	jr nz, .checkSuper
	ld de, $5C9D			; End of animation, restart
	xor a
	ld [$DA14], a
	ld a, [de]
	ld c, a
.checkSuper
	ldh a, [hSuperStatus]
	cp a, $02
	jr nz, .loadTileNumber
	ld a, c
	add a, $20				; The corresponding tile for Super Mario is always $20
	ld c, a					; tiles further
.loadTileNumber
	ld a, c
	ldi [hl], a				; tile number
	inc de
	inc l
	inc l
	dec b
	jr nz, .objectLoop
	ld a, [$DA14]
	add a, $04
	ld [$DA14], a
	ld hl, wOAMBuffer + 4 * $C + 1
	ldd a, [hl]
	cp a, $80
	jr nc, .getPrize
	add a, $04
	ldh [$FFAE], a
	ld a, [hl]
	add a, $10
	ldh [$FFAD], a
	ld bc, $DA16
	ld a, [bc]
	dec a
	ld [bc], a
	ret nz
	ld a, $01
	ld [bc], a
	call LookupTile
	ld a, [hl]
	cp a, $2E			; ladder top
	jr z, .goDownLadder
	cp a, $30
	jr z, .goUpLadder
	ret

.goDownLadder
	ld a, $18
	ldh [hGameState], a
	ret

.goUpLadder
	ld a, $19
	ldh [hGameState], a
	ret

.getPrize
	xor a
	ld [$DA1C], a
	ld a, $1A
	ldh [hGameState], a
	ret

_GameState_18:: ; 5BEB
	ld hl, wOAMBuffer + 4 * $C
	ld b, $04			; 4 objects per sprite
	ld de, $5C9D
	ld a, [$DA14]
	and a
	jr z, .objectLoop
	ld c, a
.loop
	inc de
	dec c
	jr nz, .loop
.objectLoop
	inc [hl]			; go down one pixel
	inc l
	inc l
	ld a, [de]
	ld c, a
	cp a, $FF
	jr nz, .checkSuper
	ld de, $5C9D		; reset animation
	xor a
	ld [$DA14], a
	ld a, [de]
	ld c, a
.checkSuper
	ldh a, [hSuperStatus]
	cp a, 2
	jr nz, .loadTileNumber
	ld a, c
	add a, $20
	ld c, a
.loadTileNumber
	ld a, c
	ldi [hl], a
	inc de
	inc l
	dec b
	jr nz, .objectLoop
	ld a, [$DA14]
	add a, $04
	ld [$DA14], a
	ld hl, wOAMBuffer + 4 * $C
	ld a, [hl]
	cp a, $50
	jr z, .resumeWalking
	cp a, $68
	jr z, .resumeWalking
	cp a, $80
	jr z, .resumeWalking
	ret

.resumeWalking
	ld a, $08
	ld [$DA16], a
	ld a, $17
	ldh [hGameState], a
	ret

_GameState_19:: ; 5C44
	ld hl, wOAMBuffer + 4 * $C
	ld b, $04			; 4 objects per sprite
	ld de, $5C9D
	ld a, [$DA14]
	and a
	jr z, .objectLoop
	ld c, a
.loop
	inc de
	dec c
	jr nz, .loop
.objectLoop
	dec [hl]			; go up one pixel
	inc l
	inc l
	ld a, [de]
	ld c, a
	cp a, $FF
	jr nz, .checkSuper
	ld de, $5C9D		; reset animation
	xor a
	ld [$DA14], a
	ld a, [de]
	ld c, a
.checkSuper
	ldh a, [hSuperStatus]
	cp a, 2
	jr nz, .loadTileNumber
	ld a, c
	add a, $20
	ld c, a
.loadTileNumber
	ld a, c
	ldi [hl], a
	inc de
	inc l
	dec b
	jr nz, .objectLoop
	ld a, [$DA14]
	add a, $04
	ld [$DA14], a
	ld hl, wOAMBuffer + 4 * $C
	ld a, [hl]
	cp a, $38				; top floor
	jr z, .resumeWalking
	cp a, $50				; high mid floor
	jr z, .resumeWalking
	cp a, $68				; low mid floor
	jr z, .resumeWalking
	ret

.resumeWalking
	ld a, $08
	ld [$DA16], a
	ld a, $17
	ldh [hGameState], a
	ret

; Is it possible (or even necessary) to document this more?
Data_5C9D:: ; FC9D
	db $02, $03, $12, $13
	db $02, $03, $12, $13
	db $02, $03, $12, $13
	db $02, $03, $12, $13
	db $04, $05, $14, $15
	db $04, $05, $14, $15
	db $04, $05, $14, $15
	db $04, $05, $14, $15
	db $00, $01, $16, $17
	db $00, $01, $16, $17
	db $00, $01, $16, $17
	db $00, $01, $16, $17
	db $04, $05, $14, $15
	db $04, $05, $14, $15
	db $04, $05, $14, $15
	db $04, $05, $14, $15
	db $FF

_GameState_1A:: ; 5CDE
	ld a, [$DA17]
	and a
	jp nz, .jmp_5D69
	ld c, 2
.erasePrizes
	ld hl, $98D1			; the * of the top prize
	ld de, $0060
	ld a, [wOAMBuffer + 4 * $C]	; Y coordinate
	ld b, a
	cp a, $38
	jr z, .checkHiMidFloor
	ld a, " "
	ldi [hl], a				; erase the *
	ldd [hl], a				; and the prize
.checkHiMidFloor
	add hl, de				; next floor
	ld a, b
	cp a, $50
	jr z, .checkLoMidFloor
	ld a, " "
	ldi [hl], a				; erase the *
	ldd [hl], a				; and the prize
.checkLoMidFloor
	add hl, de				; next floor
	ld a, b
	cp a, $68
	jr z, .checkBottomFloor
	ld a, " "
	ldi [hl], a				; erase the *
	ldd [hl], a				; and the prize
.checkBottomFloor
	add hl, de				; next floor
	ld a, b
	cp a, $80
	jr z, .checkPrize
	ld a, " "
	ldi [hl], a				; erase the *
	ld [hl], a				; and the prize
.checkPrize
	dec c					; erase prizes twice. todo why
	jr nz, .erasePrizes
	ld hl, wOAMBuffer + 4 * $C + 1	; X
	ldd a, [hl]
	add a, $18				; 3 tiles ahead
	ldh [$FFAE], a
	ld a, [hl]				; Y
	add a, $08				; 1 tile down
	ldh [$FFAD], a
	call LookupTile
	ld a, [hl]
	cp a, $03				; 3-UP
	jr z, .get3UP
	cp a, $E5				; Flower
	jr z, .getFlower
	cp a, $02				; 2-UP
	jr z, .get2UP
	ld a, $02				; 1-UP
	ld [$DA17], a
.playWinSound
	ld hl, $DFE8
	ld a, $0D
	ld [hl], a				; Win sound
	ret

.get2UP
	ld a, $03
	ld [$DA17], a
	jr .playWinSound

.get3UP
	ld a, $04
	ld [$DA17], a
	jr .playWinSound

.getFlower
	ldh a, [hSuperballMario]
	and a
	jr z, .awardFlower
	ld hl, $DFE8
	ld a, $0E				; Lose sound
	ld [hl], a
	ld a, $01				; no prize
	ld [$DA17], a
	ret

.awardFlower
	ld a, $10
	ld [$DA17], a
	jr .playWinSound

.jmp_5D69
	ld a, [$DA17]
	cp a, $10			; grow into superball mario
	jr nc, .jmp_5DA0
	cp a, $02			; add lives
	jp nc, .jmp_5E02
	ld a, [wBonusGameEndTimer]
	dec a
	ld [wBonusGameEndTimer], a
	ret nz
	ld a, $40
	ld [wBonusGameEndTimer], a
	xor a
	ld [$DA17], a
	ld [$DA14], a
	ld [$DA1C], a
	ld [wBonusGameGrowAnimationFlag], a
	ld [$DA20], a
	inc a
	ld [$DA16], a
	ld a, $40
	ld [wBonusGameAnimationTimer], a
	ld a, $1B
	ldh [hGameState], a		; Leave bonus game
	ret

.jmp_5DA0
	ld a, [wBonusGameAnimationTimer]
	dec a
	ld [wBonusGameAnimationTimer], a
	ret nz
	ld a, $03
	ld [wBonusGameAnimationTimer], a
	ld a, [$DA17]
	inc a
	ld [$DA17], a
	cp a, $28
	jr z, .jmp_5DF7
	ld a, [$DA1C]
	and a
	jr nz, .powerupAnimation
	inc a
	ld [$DA1C], a
	ld hl, $DFE0
	ld a, $04				; powerup sound
	ld [hl], a
	ldh a, [hSuperStatus]
	cp a, $02
	jr z, .jmp_5DF7
.powerupAnimation
	ld hl, wOAMBuffer + 4 * $C + 2	; tile number
	ld b, $04				; 4 objects
	ld a, [wBonusGameGrowAnimationFlag]
	and a
	jr nz, .makeMarioSmall
	inc a
	ld [wBonusGameGrowAnimationFlag], a
.superLoop
	ld a, [hl]
	add a, $20
	ldi [hl], a
	inc l
	inc l
	inc l
	dec b
	jr nz, .superLoop
	ret

.makeMarioSmall
	dec a
	ld [wBonusGameGrowAnimationFlag], a
.smallLoop
	ld a, [hl]
	sub a, $20
	ldi [hl], a
	inc l
	inc l
	inc l
	dec b
	jr nz, .smallLoop
	ret

.jmp_5DF7
	ld a, $01
	ld [$DA17], a
	inc a
	ldh [hSuperStatus], a
	ldh [hSuperballMario], a
	ret

.jmp_5E02
	ld a, [wBonusGameAnimationTimer]
	dec a
	ld [wBonusGameAnimationTimer], a
	ret nz
	ld a, $04
	ld [wBonusGameAnimationTimer], a
	ld a, [$DA20]
	and a
	jr nz, .jmp_5E3F
	ld hl, wOAMBuffer + 4 * $C
	ld a, $38
	ld b, a
	ldi [hl], a
	ld a, $58
	ld c, a
	ldi [hl], a
	inc l
	inc l
	ld a, b
	ldi [hl], a
	ld a, c
	add a, $08
	ldi [hl], a
	inc l
	inc l
	ld a, b
	add a, $08
	ld b, a
	ldi [hl], a
	ld a, c
	ldi [hl], a
	inc l
	inc l
	ld a, b
	ldi [hl], a
	ld a, c
	add a, $08
	ldi [hl], a
	xor a
	inc a
	ld [$DA20], a
	ret

.jmp_5E3F
	ld hl, wOAMBuffer + 4 * $C
	ld a, [$DA21]
	cp a, $02
	jp z, .jmp_5EDA
	and a
	jr nz, .jmp_5EB2
	ld a, [hl]
	dec a
	ldi [hl], a
	inc l
	ld a, $08
	ld b, a
	ldh a, [hSuperStatus]
	cp a, $02
	jr nz, .jmp_5E5E
	ld a, b
	add a, $20
	ld b, a
.jmp_5E5E
	ld a, b
	ldi [hl], a
	inc l
	ld a, [hl]
	dec a
	ldi [hl], a
	inc l
	ld a, b
	inc a
	ldi [hl], a
	inc l
	ld a, [hl]
	dec a
	ldi [hl], a
	inc l
	ld a, b
	add a, $10
	ld b, a
	ldi [hl], a
	inc l
	ld a, [hl]
	dec a
	ldi [hl], a
	inc l
	ld a, b
	inc a
	ld [hl], a
	ld a, [$DA20]
	inc a
	ld [$DA20], a
	cp a, $06
	ret nz
	ld hl, $DFE0
	ld a, $08			; 1UP sound
	ld [hl], a
	ld a, [wLives]
	and a
	cp a, $99
	jr nc, .jmp_5EA9
	add a, 1
	daa
	ld [wLives], a
	ld de, $988B		; Lives counter ones
	ld a, [wLives]
	ld b, a
	and a, $0F
	ld [de], a
	dec e				; Lives counter tens
	ld a, b
	and a, $F0
	swap a
	ld [de], a
.jmp_5EA9
	ld a, $01
	ld [$DA20], a
	ld [$DA21], a
	ret

.jmp_5EB2
	ld a, [hl]
	inc a
	ldi [hl], a
	inc l
	inc l
	inc l
	ld a, [hl]
	inc a
	ldi [hl], a
	inc l
	inc l
	inc l
	ld a, [hl]
	inc a
	ldi [hl], a
	inc l
	inc l
	inc l
	ld a, [hl]
	inc a
	ld [hl], a
	ld a, [$DA20]
	inc a
	ld [$DA20], a
	cp a, $05
	ret nz
	ld hl, $DFE0
	ld a, $02
	ld [$DA21], a
	ret

.jmp_5EDA
	ld a, [hl]
	inc a
	ldi [hl], a
	inc l
	xor a
	ld b, a
	ldh a, [hSuperStatus]
	cp a, $02
	jr nz, .jmp_5EEA
	ld a, b
	add a, $20
	ld b, a
.jmp_5EEA
	ld a, b
	ldi [hl], a
	inc l
	ld a, [hl]
	inc a
	ldi [hl], a
	inc l
	ld a, b
	inc a
	ldi [hl], a
	inc l
	ld a, [hl]
	inc a
	ldi [hl], a
	inc l
	ld a, b
	add a, $10
	ld b, a
	ldi [hl], a
	inc l
	ld a, [hl]
	inc a
	ldi [hl], a
	inc l
	ld a, b
	inc a
	ld [hl], a
	xor a
	ld [$DA20], a
	ld [$DA21], a
	ld a, [$DA17]
	dec a
	ld [$DA17], a
	ret

; Start Menu screen
;INCBIN "baserom.gb", $9F15, $6002 - $5F15
SECTION "menu screen", ROMX[$5F15], BANK[2]
Data_002_5F15::
    db   $72, $8A, $3D, $FE, $09, $30, $90, $82   ; $5F15
    db   $82, $82, $93, $6F, $8B, $3D, $FE, $02   ; $5F1D
    db   $8D, $91, $36, $50, $60, $70, $80, $89   ; $5F25
    db   $3D, $FE, $02, $8E, $41, $36, $51, $61   ; $5F2D
    db   $71, $80, $8A, $3D, $FE, $02, $8F, $41   ; $5F35
    db   $36, $52, $62, $72, $80, $8B, $3D, $E1   ; $5F3D
    db   $3E, $FE, $07, $31, $33, $43, $53, $63   ; $5F45
    db   $73, $80, $82, $3D, $1D, $E1, $01, $FE   ; $5F4D
    db   $07, $31, $34, $44, $54, $64, $74, $80   ; $5F55
    db   $82, $3D, $18, $B1, $1C, $E1, $09, $FE   ; $5F5D
    db   $07, $31, $35, $45, $55, $65, $75, $80   ; $5F65
    db   $82, $3D, $19, $B1, $1D, $E1, $08, $FE   ; $5F6D
    db   $0A, $84, $36, $46, $56, $66, $76, $88   ; $5F75
    db   $82, $3D, $29, $B1, $0A, $E1, $09, $FE   ; $5F7D
    db   $07, $85, $37, $47, $57, $67, $77, $80   ; $5F85
    db   $81, $3D, $B1, $1B, $FE, $07, $85, $38   ; $5F8D
    db   $48, $58, $68, $78, $80, $81, $3D, $B1   ; $5F95
    db   $1D, $E1, $4E, $FE, $09, $86, $39, $49   ; $5F9D
    db   $59, $69, $79, $87, $83, $3D, $E1, $4F   ; $5FA5
    db   $FE, $07, $31, $3A, $4A, $5A, $6A, $7A   ; $5FAD
    db   $80, $81, $3D, $E1, $5C, $FE, $07, $31   ; $5FB5
    db   $3B, $4B, $5B, $6B, $7B, $80, $81, $3D   ; $5FBD
    db   $E1, $5D, $FE, $02, $31, $41, $43, $6C   ; $5FC5
    db   $7C, $80, $81, $3D, $E1, $5E, $FE, $02   ; $5FCD
    db   $31, $41, $43, $6D, $7D, $80, $81, $3D   ; $5FD5
    db   $E1, $5F, $FE, $02, $31, $41, $43, $6E   ; $5FDD
    db   $7E, $80, $81, $3D, $FE, $02, $31, $92   ; $5FE5
    db   $52, $7F, $80, $81, $3D, $FE, $09, $32   ; $5FED
    db   $42, $83, $83, $83, $83, $81, $89, $3D   ; $5FF5
    db   $FE, $72, $8A, $3D, $FE

SECTION "bank 2 levels", ROMX[$6190], BANK[2]
;INCBIN "baserom.gb", $A190, $791A - $6190
MenuLevelData::
    db   $15, $5F, $BE, $62, $17, $68, $C7, $68   ; $6190
    db   $BE, $62, $00, $62, $BE, $62, $81, $63   ; $6198
    db   $5F, $64, $BE, $62, $0D, $65, $BE, $62   ; $61A0
    db   $00, $62, $81, $63, $00, $62, $BE, $62   ; $61A8
    db   $DE, $65, $B5, $66, $BB, $67, $FF, $BE   ; $61B0
    db   $62, $17, $68, $C7, $68, $A6, $69, $61   ; $61B8
    db   $6A, $23, $6B, $61, $6A, $F5, $6B, $AB   ; $61C0
    db   $6C, $23, $6B, $23, $6B, $29, $6D, $AD   ; $61C8
    db   $6D, $F5, $6B, $AB, $6C, $A6, $69, $BB   ; $61D0
    db   $67, $FF, $BE, $62, $CA, $76, $9F, $77   ; $61D8
    db   $2F, $6E, $21, $6F, $F2, $6F, $F2, $6F   ; $61E0
    db   $FD, $70, $21, $6F, $2F, $6E, $2F, $6E   ; $61E8
    db   $53, $72, $F2, $72, $E0, $73, $C8, $74   ; $61F0
    db   $87, $71, $87, $71, $C6, $75, $FF, $00   ; $61F8
    db   $02, $53, $40, $E2, $60, $61, $FE, $02   ; $6200
    db   $53, $40, $E2, $60, $61, $FE, $02, $53   ; $6208
    db   $40, $91, $81, $E2, $60, $61, $FE, $02   ; $6210
    db   $53, $40, $E2, $60, $61, $FE, $02, $53   ; $6218
    db   $40, $E2, $60, $61, $FE, $02, $53, $40   ; $6220
    db   $E2, $60, $61, $FE, $02, $53, $40, $E2   ; $6228
    db   $60, $61, $FE, $02, $53, $40, $C4, $70   ; $6230
    db   $72, $60, $61, $FE, $02, $53, $40, $C4   ; $6238
    db   $71, $73, $60, $61, $FE, $02, $53, $40   ; $6240
    db   $C1, $36, $E2, $60, $61, $FE, $02, $53   ; $6248
    db   $40, $97, $32, $52, $34, $52, $52, $60   ; $6250
    db   $61, $FE, $02, $53, $40, $92, $33, $36   ; $6258
    db   $E2, $60, $61, $FE, $02, $53, $40, $91   ; $6260
    db   $36, $E2, $60, $61, $FE, $02, $53, $40   ; $6268
    db   $81, $36, $E2, $60, $61, $FE, $02, $53   ; $6270
    db   $40, $61, $81, $81, $5E, $B5, $63, $63   ; $6278
    db   $63, $60, $61, $FE, $02, $53, $40, $91   ; $6280
    db   $5E, $B5, $63, $63, $63, $60, $61, $FE   ; $6288
    db   $02, $53, $40, $41, $44, $A1, $5E, $E2   ; $6290
    db   $60, $61, $FE, $02, $53, $40, $32, $41   ; $6298
    db   $45, $B1, $5E, $E2, $60, $61, $FE, $02   ; $62A0
    db   $53, $40, $32, $42, $46, $C1, $5E, $E2   ; $62A8
    db   $60, $61, $FE, $02, $53, $40, $32, $43   ; $62B0
    db   $47, $D3, $5E, $60, $61, $FE, $02, $53   ; $62B8
    db   $40, $D3, $36, $60, $61, $FE, $02, $53   ; $62C0
    db   $40, $C4, $70, $72, $60, $61, $FE, $02   ; $62C8
    db   $53, $40, $B5, $36, $71, $73, $60, $61   ; $62D0
    db   $FE, $02, $53, $40, $B1, $5E, $E2, $60   ; $62D8
    db   $61, $FE, $02, $53, $40, $B2, $36, $5E   ; $62E0
    db   $E2, $60, $61, $FE, $02, $53, $40, $92   ; $62E8
    db   $81, $36, $D3, $5E, $60, $61, $FE, $02   ; $62F0
    db   $53, $40, $91, $36, $E2, $60, $61, $FE   ; $62F8
    db   $02, $53, $40, $81, $36, $C4, $32, $31   ; $6300
    db   $60, $61, $FE, $02, $53, $40, $71, $36   ; $6308
    db   $C1, $33, $E2, $60, $61, $FE, $02, $53   ; $6310
    db   $40, $71, $5E, $B5, $32, $31, $31, $60   ; $6318
    db   $61, $FE, $02, $53, $40, $81, $5E, $B1   ; $6320
    db   $33, $E2, $60, $61, $FE, $02, $53, $40   ; $6328
    db   $31, $44, $91, $5E, $E2, $60, $61, $FE   ; $6330
    db   $04, $53, $40, $41, $45, $A1, $5E, $E2   ; $6338
    db   $60, $61, $FE, $04, $53, $40, $42, $46   ; $6340
    db   $B1, $5E, $E2, $60, $61, $FE, $04, $53   ; $6348
    db   $40, $41, $45, $C1, $5E, $E2, $60, $61   ; $6350
    db   $FE, $04, $53, $40, $42, $46, $D3, $5E   ; $6358
    db   $60, $61, $FE, $04, $53, $40, $43, $47   ; $6360
    db   $E2, $60, $61, $FE, $02, $53, $40, $E2   ; $6368
    db   $60, $61, $FE, $02, $53, $40, $E2, $60   ; $6370
    db   $61, $FE, $02, $53, $40, $E2, $60, $61   ; $6378
    db   $FE, $02, $53, $40, $B5, $52, $52, $52   ; $6380
    db   $60, $61, $FE, $02, $53, $40, $A6, $36   ; $6388
    db   $60, $E8, $E8, $E8, $61, $FE, $02, $53   ; $6390
    db   $40, $A6, $5E, $60, $E8, $E8, $E8, $61   ; $6398
    db   $FE, $02, $53, $40, $B5, $60, $E8, $E8   ; $63A0
    db   $E8, $61, $FE, $02, $53, $40, $A6, $36   ; $63A8
    db   $60, $E8, $E8, $E8, $61, $FE, $02, $53   ; $63B0
    db   $40, $A6, $5E, $60, $E8, $E8, $E8, $61   ; $63B8
    db   $FE, $02, $53, $40, $B5, $60, $E8, $E8   ; $63C0
    db   $E8, $61, $FE, $02, $53, $40, $71, $81   ; $63C8
    db   $B5, $60, $E8, $E8, $E8, $61, $FE, $02   ; $63D0
    db   $53, $40, $71, $82, $B5, $60, $E8, $E8   ; $63D8
    db   $E8, $61, $FE, $02, $53, $40, $71, $81   ; $63E0
    db   $B5, $60, $E8, $E8, $E8, $61, $FE, $02   ; $63E8
    db   $53, $40, $71, $82, $B5, $60, $E8, $E8   ; $63F0
    db   $E8, $61, $FE, $02, $53, $40, $71, $81   ; $63F8
    db   $B5, $60, $E8, $E8, $E8, $61, $FE, $02   ; $6400
    db   $53, $40, $B5, $60, $E8, $E8, $E8, $61   ; $6408
    db   $FE, $02, $53, $40, $B5, $60, $E8, $E8   ; $6410
    db   $E8, $61, $FE, $02, $53, $40, $B5, $60   ; $6418
    db   $E8, $E8, $E8, $61, $FE, $02, $53, $40   ; $6420
    db   $B5, $60, $E8, $E8, $E8, $61, $FE, $02   ; $6428
    db   $53, $40, $B5, $60, $E8, $E8, $E8, $61   ; $6430
    db   $FE, $02, $53, $40, $B5, $60, $E8, $E8   ; $6438
    db   $E8, $61, $FE, $02, $53, $40, $79, $70   ; $6440
    db   $72, $72, $72, $60, $E8, $E8, $E8, $61   ; $6448
    db   $FE, $02, $53, $40, $79, $71, $73, $73   ; $6450
    db   $73, $60, $E8, $E8, $E8, $61, $FE, $02   ; $6458
    db   $53, $40, $51, $82, $E2, $60, $61, $FE   ; $6460
    db   $02, $53, $40, $51, $82, $C4, $32, $31   ; $6468
    db   $60, $61, $FE, $02, $53, $40, $51, $82   ; $6470
    db   $A1, $81, $C1, $33, $E2, $60, $61, $FE   ; $6478
    db   $02, $53, $40, $51, $80, $B5, $32, $31   ; $6480
    db   $31, $60, $61, $FE, $02, $53, $40, $51   ; $6488
    db   $82, $B1, $33, $E2, $60, $61, $FE, $02   ; $6490
    db   $53, $40, $51, $82, $E2, $60, $61, $FE   ; $6498
    db   $02, $53, $40, $E2, $60, $61, $FE, $02   ; $64A0
    db   $53, $40, $37, $FD, $F4, $E2, $60, $61   ; $64A8
    db   $FE, $02, $53, $40, $E2, $60, $61, $FE   ; $64B0
    db   $02, $53, $40, $E2, $FD, $50, $FE, $02   ; $64B8
    db   $53, $40, $E2, $FD, $52, $FE, $02, $53   ; $64C0
    db   $40, $E2, $60, $61, $FE, $02, $53, $40   ; $64C8
    db   $E2, $60, $61, $FE, $02, $53, $40, $E2   ; $64D0
    db   $60, $61, $FE, $02, $53, $40, $A1, $82   ; $64D8
    db   $E2, $60, $61, $FE, $02, $53, $40, $61   ; $64E0
    db   $81, $A1, $82, $E2, $60, $61, $FE, $02   ; $64E8
    db   $53, $40, $A1, $82, $E2, $60, $61, $FE   ; $64F0
    db   $02, $53, $40, $E2, $60, $61, $FE, $02   ; $64F8
    db   $53, $40, $E2, $60, $61, $FE, $02, $53   ; $6500
    db   $40, $E2, $60, $61, $FE, $02, $53, $40   ; $6508
    db   $D3, $36, $60, $61, $FE, $02, $53, $40   ; $6510
    db   $C1, $36, $E2, $60, $61, $FE, $02, $53   ; $6518
    db   $40, $B1, $36, $E2, $60, $61, $FE, $02   ; $6520
    db   $53, $40, $A1, $36, $E2, $60, $61, $FE   ; $6528
    db   $02, $53, $40, $92, $36, $7F, $E2, $60   ; $6530
    db   $61, $FE, $02, $53, $40, $81, $36, $A1   ; $6538
    db   $7F, $E2, $60, $61, $FE, $02, $53, $40   ; $6540
    db   $62, $81, $36, $A1, $7F, $E2, $60, $61   ; $6548
    db   $FE, $02, $53, $40, $61, $81, $A1, $7F   ; $6550
    db   $E2, $60, $61, $FE, $03, $53, $40, $81   ; $6558
    db   $52, $36, $81, $A1, $7F, $E2, $60, $61   ; $6560
    db   $FE, $03, $53, $40, $81, $52, $5E, $81   ; $6568
    db   $A1, $7F, $E2, $60, $61, $FE, $02, $53   ; $6570
    db   $40, $61, $81, $A1, $7F, $E2, $60, $61   ; $6578
    db   $FE, $02, $53, $40, $62, $81, $5E, $A1   ; $6580
    db   $7F, $E2, $60, $61, $FE, $02, $53, $40   ; $6588
    db   $81, $5E, $A1, $7F, $E2, $60, $61, $FE   ; $6590
    db   $02, $53, $40, $92, $5E, $7F, $E2, $60   ; $6598
    db   $61, $FE, $02, $53, $40, $A1, $5E, $C4   ; $65A0
    db   $32, $31, $60, $61, $FE, $02, $53, $40   ; $65A8
    db   $B2, $5E, $33, $E2, $60, $61, $FE, $02   ; $65B0
    db   $53, $40, $31, $44, $B5, $32, $3C, $31   ; $65B8
    db   $60, $61, $FE, $04, $53, $40, $41, $45   ; $65C0
    db   $B1, $33, $D3, $5E, $60, $61, $FE, $04   ; $65C8
    db   $53, $40, $42, $46, $E2, $FD, $50, $FE   ; $65D0
    db   $04, $53, $40, $43, $47, $FE, $02, $53   ; $65D8
    db   $40, $B5, $32, $31, $31, $60, $61, $FE   ; $65E0
    db   $02, $53, $40, $B1, $33, $D3, $36, $60   ; $65E8
    db   $61, $FE, $02, $53, $40, $A6, $32, $31   ; $65F0
    db   $34, $31, $60, $61, $FE, $02, $53, $40   ; $65F8
    db   $A2, $33, $36, $E2, $60, $61, $FE, $02   ; $6600
    db   $53, $40, $B1, $5E, $E2, $60, $61, $FE   ; $6608
    db   $02, $53, $40, $B2, $36, $5E, $E2, $60   ; $6610
    db   $61, $FE, $02, $53, $40, $A1, $36, $D3   ; $6618
    db   $5E, $60, $61, $FE, $02, $53, $40, $91   ; $6620
    db   $36, $E2, $FD, $50, $FE, $02, $53, $40   ; $6628
    db   $81, $36, $FE, $02, $53, $40, $71, $36   ; $6630
    db   $E2, $FD, $52, $FE, $02, $53, $40, $61   ; $6638
    db   $36, $A6, $48, $4A, $63, $63, $60, $61   ; $6640
    db   $FE, $02, $53, $40, $51, $36, $A6, $49   ; $6648
    db   $4B, $63, $63, $60, $61, $FE, $02, $53   ; $6650
    db   $40, $51, $5E, $B5, $4C, $63, $63, $60   ; $6658
    db   $61, $FE, $02, $53, $40, $61, $5E, $C4   ; $6660
    db   $63, $63, $60, $61, $FE, $02, $53, $40   ; $6668
    db   $71, $5E, $C4, $63, $63, $60, $61, $FE   ; $6670
    db   $02, $53, $40, $81, $5E, $B5, $63, $63   ; $6678
    db   $63, $60, $61, $FE, $02, $53, $40, $91   ; $6680
    db   $5E, $B5, $63, $63, $63, $60, $61, $FE   ; $6688
    db   $02, $53, $40, $71, $82, $A6, $5E, $63   ; $6690
    db   $63, $63, $60, $61, $FE, $02, $53, $40   ; $6698
    db   $71, $82, $B5, $63, $63, $63, $60, $61   ; $66A0
    db   $FE, $02, $53, $40, $71, $80, $B5, $63   ; $66A8
    db   $63, $63, $60, $61, $FE, $02, $53, $40   ; $66B0
    db   $31, $44, $B5, $63, $63, $63, $60, $61   ; $66B8
    db   $FE, $04, $53, $40, $41, $45, $B1, $36   ; $66C0
    db   $E2, $FD, $50, $FE, $04, $53, $40, $42   ; $66C8
    db   $46, $A1, $36, $E2, $FD, $52, $FE, $04   ; $66D0
    db   $53, $40, $43, $47, $91, $36, $D3, $63   ; $66D8
    db   $60, $61, $FE, $02, $53, $40, $81, $36   ; $66E0
    db   $C4, $63, $63, $60, $61, $FE, $02, $53   ; $66E8
    db   $40, $71, $36, $C4, $63, $63, $60, $61   ; $66F0
    db   $FE, $02, $53, $40, $61, $36, $A6, $63   ; $66F8
    db   $F4, $63, $63, $60, $61, $FE, $02, $53   ; $6700
    db   $40, $51, $36, $92, $FD, $63, $C4, $63   ; $6708
    db   $63, $60, $61, $FE, $02, $53, $40, $51   ; $6710
    db   $5E, $97, $63, $63, $F4, $63, $63, $60   ; $6718
    db   $61, $FE, $02, $53, $40, $62, $5E, $63   ; $6720
    db   $92, $FD, $63, $C4, $63, $63, $60, $61   ; $6728
    db   $FE, $02, $53, $40, $6A, $63, $63, $F4   ; $6730
    db   $63, $63, $F4, $63, $63, $60, $61, $FE   ; $6738
    db   $02, $53, $40, $5B, $63, $63, $63, $5E   ; $6740
    db   $63, $63, $63, $63, $63, $60, $61, $FE   ; $6748
    db   $02, $53, $40, $4C, $63, $63, $63, $63   ; $6750
    db   $F4, $63, $63, $63, $63, $63, $60, $61   ; $6758
    db   $FE, $02, $53, $40, $35, $FD, $63, $97   ; $6760
    db   $63, $63, $63, $63, $63, $60, $61, $FE   ; $6768
    db   $00, $53, $40, $63, $63, $63, $63, $63   ; $6770
    db   $63, $F4, $63, $63, $63, $63, $63, $60   ; $6778
    db   $61, $FE, $08, $53, $40, $63, $63, $63   ; $6780
    db   $63, $63, $63, $97, $63, $63, $63, $63   ; $6788
    db   $63, $60, $61, $FE, $02, $53, $40, $D3   ; $6790
    db   $5E, $60, $61, $FE, $02, $53, $40, $31   ; $6798
    db   $F4, $51, $F4, $71, $F4, $91, $F4, $B1   ; $67A0
    db   $F4, $E2, $60, $61, $FE, $02, $53, $40   ; $67A8
    db   $E2, $60, $61, $FE, $02, $53, $40, $E2   ; $67B0
    db   $60, $61, $FE, $F1, $8E, $FE, $F1, $8F   ; $67B8
    db   $FE, $F1, $8E, $FE, $F1, $8F, $FE, $F1   ; $67C0
    db   $8E, $FE, $F1, $8F, $FE, $F1, $8E, $FE   ; $67C8
    db   $F1, $8F, $FE, $F1, $8E, $FE, $F1, $8F   ; $67D0
    db   $FE, $F1, $8E, $FE, $F1, $8F, $FE, $F1   ; $67D8
    db   $8E, $FE, $F1, $8F, $FE, $F1, $8E, $FE   ; $67E0
    db   $F1, $8F, $FE, $F1, $8E, $FE, $21, $8E   ; $67E8
    db   $F1, $8F, $FE, $00, $13, $24, $8F, $8E   ; $67F0
    db   $8E, $8E, $8E, $8E, $8E, $8E, $8E, $8E   ; $67F8
    db   $8E, $13, $24, $8E, $FE, $00, $21, $39   ; $6800
    db   $8E, $8F, $8F, $8F, $8F, $8F, $8F, $8F   ; $6808
    db   $8F, $8F, $8F, $21, $39, $8F, $FE, $00   ; $6810
    db   $FD, $7F, $FE, $E2, $FD, $7F, $FE, $E2   ; $6818
    db   $FD, $7F, $FE, $E2, $FD, $7F, $FE, $01   ; $6820
    db   $7F, $E2, $FD, $7F, $FE, $01, $7F, $C4   ; $6828
    db   $FD, $7F, $FE, $01, $7F, $71, $F4, $91   ; $6830
    db   $F4, $B5, $F4, $7F, $7F, $7F, $7F, $FE   ; $6838
    db   $01, $7F, $71, $F4, $91, $F4, $B5, $F4   ; $6840
    db   $7F, $7F, $7F, $7F, $FE, $01, $7F, $71   ; $6848
    db   $F4, $91, $F4, $B5, $F4, $7F, $7F, $7F   ; $6850
    db   $7F, $FE, $01, $7F, $71, $F4, $91, $F4   ; $6858
    db   $B5, $F4, $7F, $7F, $7F, $7F, $FE, $01   ; $6860
    db   $7F, $71, $F4, $91, $F4, $B5, $F4, $7F   ; $6868
    db   $7F, $7F, $7F, $FE, $01, $7F, $71, $F4   ; $6870
    db   $91, $F4, $B5, $F4, $7F, $7F, $7F, $7F   ; $6878
    db   $FE, $01, $7F, $C4, $FD, $7F, $FE, $01   ; $6880
    db   $7F, $E2, $FD, $7F, $FE, $01, $7F, $E2   ; $6888
    db   $FD, $7F, $FE, $E2, $FD, $7F, $FE, $C4   ; $6890
    db   $74, $77, $7F, $7F, $FE, $C4, $75, $78   ; $6898
    db   $7F, $7F, $FE, $00, $72, $72, $72, $72   ; $68A0
    db   $72, $72, $72, $72, $72, $72, $72, $72   ; $68A8
    db   $76, $79, $7F, $7F, $FE, $00, $73, $73   ; $68B0
    db   $73, $73, $73, $73, $73, $73, $73, $73   ; $68B8
    db   $73, $73, $73, $73, $7F, $7F, $FE, $00   ; $68C0
    db   $FD, $7F, $FE, $E2, $FD, $7F, $FE, $C1   ; $68C8
    db   $F4, $E2, $FD, $7F, $FE, $E2, $FD, $7F   ; $68D0
    db   $FE, $08, $FD, $7F, $C1, $F4, $E2, $FD   ; $68D8
    db   $7F, $FE, $01, $7F, $31, $F4, $74, $FD   ; $68E0
    db   $7F, $E2, $FD, $7F, $FE, $01, $7F, $81   ; $68E8
    db   $F4, $A1, $7F, $C1, $F4, $E2, $FD, $7F   ; $68F0
    db   $FE, $01, $7F, $31, $F4, $51, $7F, $A1   ; $68F8
    db   $7F, $E2, $FD, $7F, $FE, $01, $7F, $51   ; $6900
    db   $7F, $81, $F4, $A1, $7F, $C1, $F4, $E2   ; $6908
    db   $FD, $7F, $FE, $01, $7F, $31, $F4, $51   ; $6910
    db   $7F, $A1, $7F, $E2, $FD, $7F, $FE, $01   ; $6918
    db   $7F, $51, $7F, $81, $F4, $A1, $7F, $C1   ; $6920
    db   $F4, $E2, $FD, $7F, $FE, $01, $7F, $31   ; $6928
    db   $F4, $51, $7F, $A1, $7F, $E2, $FD, $7F   ; $6930
    db   $FE, $01, $7F, $51, $7F, $81, $F4, $A1   ; $6938
    db   $7F, $C1, $F4, $E2, $FD, $7F, $FE, $01   ; $6940
    db   $7F, $31, $F4, $51, $7F, $A1, $7F, $E2   ; $6948
    db   $FD, $7F, $FE, $01, $7F, $51, $7F, $81   ; $6950
    db   $F4, $A1, $7F, $C1, $F4, $E2, $FD, $7F   ; $6958
    db   $FE, $01, $7F, $31, $F4, $51, $7F, $A1   ; $6960
    db   $7F, $E2, $FD, $7F, $FE, $01, $7F, $33   ; $6968
    db   $74, $77, $7F, $81, $F4, $A1, $7F, $C1   ; $6970
    db   $F4, $E2, $FD, $7F, $FE, $01, $7F, $33   ; $6978
    db   $75, $78, $7F, $E2, $FD, $7F, $FE, $06   ; $6980
    db   $72, $72, $72, $76, $79, $7F, $C1, $F4   ; $6988
    db   $E2, $FD, $7F, $FE, $00, $73, $73, $73   ; $6990
    db   $73, $73, $7F, $7F, $7F, $7F, $7F, $7F   ; $6998
    db   $7F, $7F, $7F, $7F, $7F, $FE, $02, $53   ; $69A0
    db   $40, $FE, $02, $53, $40, $E2, $FD, $52   ; $69A8
    db   $FE, $02, $53, $40, $41, $44, $B5, $32   ; $69B0
    db   $31, $31, $60, $61, $FE, $02, $53, $40   ; $69B8
    db   $32, $41, $45, $B1, $33, $E2, $60, $61   ; $69C0
    db   $FE, $02, $53, $40, $32, $42, $46, $88   ; $69C8
    db   $32, $31, $31, $31, $31, $31, $60, $61   ; $69D0
    db   $FE, $02, $53, $40, $32, $43, $47, $88   ; $69D8
    db   $33, $32, $31, $31, $31, $31, $60, $61   ; $69E0
    db   $FE, $02, $53, $40, $91, $33, $E2, $60   ; $69E8
    db   $61, $FE, $02, $53, $40, $E2, $60, $61   ; $69F0
    db   $FE, $02, $53, $40, $E2, $60, $61, $FE   ; $69F8
    db   $02, $53, $40, $E2, $FD, $50, $FE, $02   ; $6A00
    db   $53, $40, $B1, $68, $FE, $02, $53, $40   ; $6A08
    db   $B1, $69, $FE, $02, $53, $40, $31, $44   ; $6A10
    db   $B5, $69, $37, $37, $37, $37, $FE, $04   ; $6A18
    db   $53, $40, $41, $45, $B5, $69, $37, $37   ; $6A20
    db   $37, $37, $FE, $04, $53, $40, $42, $46   ; $6A28
    db   $B1, $69, $FE, $04, $53, $40, $43, $47   ; $6A30
    db   $71, $68, $B1, $6A, $FE, $02, $53, $40   ; $6A38
    db   $51, $F4, $71, $69, $FE, $02, $53, $40   ; $6A40
    db   $79, $69, $37, $37, $37, $37, $37, $37   ; $6A48
    db   $37, $37, $FE, $02, $53, $40, $51, $F4   ; $6A50
    db   $71, $69, $FE, $02, $53, $40, $71, $6A   ; $6A58
    db   $FE, $02, $53, $40, $31, $F4, $71, $68   ; $6A60
    db   $FE, $02, $53, $40, $31, $81, $79, $69   ; $6A68
    db   $37, $37, $37, $37, $37, $37, $37, $37   ; $6A70
    db   $FE, $02, $53, $40, $31, $F4, $71, $6A   ; $6A78
    db   $F1, $36, $FE, $02, $53, $40, $B1, $68   ; $6A80
    db   $E1, $36, $FE, $02, $53, $40, $B1, $69   ; $6A88
    db   $E1, $5E, $FE, $02, $53, $40, $B1, $69   ; $6A90
    db   $E2, $36, $5E, $FE, $02, $53, $40, $B5   ; $6A98
    db   $69, $37, $37, $37, $37, $FE, $02, $53   ; $6AA0
    db   $40, $B2, $69, $36, $FE, $02, $53, $40   ; $6AA8
    db   $B2, $69, $5E, $FE, $02, $53, $40, $41   ; $6AB0
    db   $44, $B1, $6A, $D1, $5E, $FE, $02, $53   ; $6AB8
    db   $40, $32, $41, $45, $91, $68, $E1, $5E   ; $6AC0
    db   $FE, $02, $53, $40, $32, $42, $46, $91   ; $6AC8
    db   $69, $F1, $5E, $FE, $02, $53, $40, $32   ; $6AD0
    db   $43, $47, $91, $69, $FE, $02, $53, $40   ; $6AD8
    db   $51, $F4, $97, $69, $37, $37, $37, $37   ; $6AE0
    db   $37, $37, $FE, $02, $53, $40, $91, $69   ; $6AE8
    db   $F1, $36, $FE, $02, $53, $40, $91, $69   ; $6AF0
    db   $E1, $36, $FE, $02, $53, $40, $91, $6A   ; $6AF8
    db   $D1, $36, $FE, $02, $53, $40, $71, $68   ; $6B00
    db   $D1, $5E, $FE, $02, $53, $40, $31, $F4   ; $6B08
    db   $79, $69, $37, $37, $37, $37, $37, $37   ; $6B10
    db   $37, $37, $FE, $02, $53, $40, $71, $6A   ; $6B18
    db   $F1, $5E, $FE, $02, $53, $40, $71, $68   ; $6B20
    db   $FE, $02, $53, $40, $71, $69, $FE, $02   ; $6B28
    db   $53, $40, $71, $69, $FE, $02, $53, $40   ; $6B30
    db   $71, $69, $FE, $02, $53, $40, $71, $69   ; $6B38
    db   $E2, $32, $31, $FE, $02, $53, $40, $71   ; $6B40
    db   $69, $E1, $33, $FE, $02, $53, $40, $71   ; $6B48
    db   $69, $FE, $02, $53, $40, $79, $69, $37   ; $6B50
    db   $37, $37, $37, $37, $37, $37, $37, $FE   ; $6B58
    db   $02, $53, $40, $79, $69, $37, $37, $37   ; $6B60
    db   $37, $37, $37, $37, $37, $FE, $02, $53   ; $6B68
    db   $40, $79, $69, $37, $37, $37, $37, $37   ; $6B70
    db   $37, $37, $37, $FE, $02, $53, $40, $31   ; $6B78
    db   $44, $79, $69, $37, $37, $37, $37, $37   ; $6B80
    db   $37, $37, $37, $FE, $04, $53, $40, $41   ; $6B88
    db   $45, $79, $69, $37, $37, $37, $37, $37   ; $6B90
    db   $37, $37, $37, $FE, $04, $53, $40, $42   ; $6B98
    db   $46, $79, $69, $37, $37, $37, $37, $37   ; $6BA0
    db   $37, $37, $37, $FE, $04, $53, $40, $41   ; $6BA8
    db   $45, $71, $69, $E1, $36, $FE, $04, $53   ; $6BB0
    db   $40, $42, $46, $71, $69, $E1, $5E, $FE   ; $6BB8
    db   $04, $53, $40, $43, $47, $71, $69, $E2   ; $6BC0
    db   $36, $5E, $FE, $02, $53, $40, $71, $69   ; $6BC8
    db   $D1, $36, $F1, $32, $FE, $02, $53, $40   ; $6BD0
    db   $71, $69, $D1, $5E, $F1, $33, $FE, $02   ; $6BD8
    db   $53, $40, $71, $69, $B5, $32, $31, $31   ; $6BE0
    db   $3C, $31, $FE, $02, $53, $40, $71, $6A   ; $6BE8
    db   $B1, $33, $F1, $5E, $FE, $02, $53, $40   ; $6BF0
    db   $FE, $02, $53, $40, $FE, $02, $53, $40   ; $6BF8
    db   $FE, $02, $53, $40, $51, $F4, $71, $F4   ; $6C00
    db   $91, $F4, $E1, $68, $FE, $02, $53, $40   ; $6C08
    db   $E1, $69, $FE, $02, $53, $40, $E2, $69   ; $6C10
    db   $37, $FE, $02, $53, $40, $E1, $69, $FE   ; $6C18
    db   $02, $53, $40, $A1, $68, $E1, $6A, $FE   ; $6C20
    db   $02, $53, $40, $A1, $69, $FE, $02, $53   ; $6C28
    db   $40, $A6, $69, $37, $37, $37, $37, $37   ; $6C30
    db   $FE, $02, $53, $40, $71, $68, $A1, $69   ; $6C38
    db   $F1, $36, $FE, $02, $53, $40, $71, $69   ; $6C40
    db   $A1, $6A, $E1, $36, $FE, $02, $53, $40   ; $6C48
    db   $31, $81, $71, $69, $E1, $5E, $FE, $02   ; $6C50
    db   $53, $40, $31, $82, $71, $69, $E2, $36   ; $6C58
    db   $5E, $FE, $02, $53, $40, $31, $81, $79   ; $6C60
    db   $69, $37, $37, $37, $37, $37, $37, $37   ; $6C68
    db   $37, $FE, $02, $53, $40, $31, $82, $71   ; $6C70
    db   $69, $B2, $5F, $36, $E1, $68, $FE, $02   ; $6C78
    db   $53, $40, $31, $81, $71, $69, $A6, $32   ; $6C80
    db   $52, $3C, $52, $69, $52, $FE, $02, $53   ; $6C88
    db   $40, $71, $69, $A1, $33, $D3, $5E, $69   ; $6C90
    db   $37, $FE, $02, $53, $40, $71, $6A, $E1   ; $6C98
    db   $69, $FE, $02, $53, $40, $B1, $81, $E2   ; $6CA0
    db   $6A, $5E, $FE, $02, $53, $40, $71, $68   ; $6CA8
    db   $FE, $02, $53, $40, $31, $44, $79, $69   ; $6CB0
    db   $37, $37, $37, $37, $37, $37, $37, $37   ; $6CB8
    db   $FE, $04, $53, $40, $41, $45, $71, $6A   ; $6CC0
    db   $FE, $04, $53, $40, $42, $46, $FE, $04   ; $6CC8
    db   $53, $40, $43, $47, $FE, $02, $53, $40   ; $6CD0
    db   $C4, $32, $31, $31, $31, $FE, $02, $53   ; $6CD8
    db   $40, $C1, $33, $FE, $02, $53, $40, $E2   ; $6CE0
    db   $32, $31, $FE, $02, $53, $40, $E1, $33   ; $6CE8
    db   $FE, $02, $53, $40, $FE, $02, $53, $40   ; $6CF0
    db   $FE, $02, $53, $40, $FE, $02, $53, $40   ; $6CF8
    db   $FE, $02, $53, $40, $FE, $02, $53, $40   ; $6D00
    db   $41, $F4, $FE, $02, $53, $40, $41, $F4   ; $6D08
    db   $FE, $02, $53, $40, $41, $F4, $FE, $02   ; $6D10
    db   $53, $40, $81, $7F, $FE, $02, $53, $40   ; $6D18
    db   $81, $7F, $FE, $02, $53, $40, $81, $7F   ; $6D20
    db   $FE, $02, $53, $40, $31, $F4, $FE, $02   ; $6D28
    db   $53, $40, $FE, $02, $53, $40, $31, $F4   ; $6D30
    db   $E1, $68, $FE, $02, $53, $40, $E1, $69   ; $6D38
    db   $FE, $02, $53, $40, $E1, $69, $FE, $02   ; $6D40
    db   $53, $40, $E2, $69, $37, $FE, $02, $53   ; $6D48
    db   $40, $E1, $69, $FE, $02, $53, $40, $E1   ; $6D50
    db   $69, $FE, $02, $53, $40, $E1, $6A, $FE   ; $6D58
    db   $02, $53, $40, $31, $44, $FE, $04, $53   ; $6D60
    db   $40, $41, $45, $FE, $04, $53, $40, $42   ; $6D68
    db   $46, $FE, $04, $53, $40, $43, $47, $F1   ; $6D70
    db   $63, $FE, $02, $53, $40, $E2, $FD, $63   ; $6D78
    db   $FE, $02, $53, $40, $D3, $FD, $63, $FE   ; $6D80
    db   $02, $53, $40, $C4, $FD, $63, $FE, $02   ; $6D88
    db   $53, $40, $B5, $FD, $63, $FE, $02, $53   ; $6D90
    db   $40, $A6, $FD, $63, $FE, $02, $53, $40   ; $6D98
    db   $97, $FD, $63, $FE, $02, $53, $40, $51   ; $6DA0
    db   $81, $97, $FD, $63, $FE, $02, $53, $40   ; $6DA8
    db   $97, $FD, $63, $FE, $02, $53, $40, $31   ; $6DB0
    db   $44, $A6, $FD, $63, $FE, $04, $53, $40   ; $6DB8
    db   $41, $45, $B5, $FD, $63, $FE, $04, $53   ; $6DC0
    db   $40, $42, $46, $F1, $36, $FE, $04, $53   ; $6DC8
    db   $40, $41, $45, $D3, $FD, $63, $FE, $04   ; $6DD0
    db   $53, $40, $42, $46, $D3, $36, $63, $63   ; $6DD8
    db   $FE, $04, $53, $40, $43, $47, $C1, $36   ; $6DE0
    db   $F1, $63, $FE, $02, $53, $40, $C1, $5E   ; $6DE8
    db   $FE, $02, $53, $40, $D1, $5E, $FE, $02   ; $6DF0
    db   $53, $40, $E1, $5E, $FE, $02, $53, $40   ; $6DF8
    db   $F1, $5E, $FE, $02, $53, $40, $FE, $02   ; $6E00
    db   $53, $40, $E2, $32, $31, $FE, $02, $53   ; $6E08
    db   $40, $E2, $33, $32, $FE, $02, $53, $40   ; $6E10
    db   $F1, $33, $FE, $02, $53, $40, $FE, $02   ; $6E18
    db   $53, $40, $FE, $02, $53, $40, $FE, $02   ; $6E20
    db   $53, $40, $FE, $02, $53, $40, $FE, $0B   ; $6E28
    db   $65, $6E, $6D, $6E, $6D, $65, $65, $65   ; $6E30
    db   $65, $65, $65, $E2, $65, $6E, $FE, $0B   ; $6E38
    db   $66, $6D, $6E, $6D, $6E, $66, $66, $66   ; $6E40
    db   $66, $66, $66, $E2, $66, $6D, $FE, $06   ; $6E48
    db   $65, $6E, $6D, $6E, $59, $5B, $91, $5F   ; $6E50
    db   $E2, $65, $6E, $FE, $06, $66, $6D, $6E   ; $6E58
    db   $6D, $5A, $5C, $E2, $66, $6D, $FE, $05   ; $6E60
    db   $65, $6E, $6D, $66, $55, $E2, $65, $6E   ; $6E68
    db   $FE, $03, $66, $6D, $6E, $41, $56, $62   ; $6E70
    db   $5D, $35, $E2, $66, $6D, $FE, $03, $65   ; $6E78
    db   $6E, $6D, $41, $57, $E2, $65, $6E, $FE   ; $6E80
    db   $03, $66, $6D, $66, $42, $58, $2B, $A1   ; $6E88
    db   $81, $E2, $66, $6D, $FE, $02, $65, $6E   ; $6E90
    db   $42, $59, $5B, $A1, $82, $E2, $65, $6E   ; $6E98
    db   $FE, $02, $66, $6D, $42, $5A, $5C, $A1   ; $6EA0
    db   $81, $E2, $66, $6D, $FE, $02, $65, $6E   ; $6EA8
    db   $42, $57, $55, $92, $5F, $82, $E2, $65   ; $6EB0
    db   $6E, $FE, $02, $66, $6D, $A1, $81, $E2   ; $6EB8
    db   $66, $6D, $FE, $02, $65, $6E, $42, $56   ; $6EC0
    db   $57, $E2, $65, $6E, $FE, $02, $6E, $6D   ; $6EC8
    db   $41, $2B, $C4, $65, $65, $6E, $6D, $FE   ; $6ED0
    db   $02, $6D, $6E, $41, $55, $62, $5D, $35   ; $6ED8
    db   $C4, $66, $66, $6D, $6E, $FE, $02, $66   ; $6EE0
    db   $6D, $41, $57, $E2, $66, $6D, $FE, $03   ; $6EE8
    db   $65, $6E, $65, $42, $38, $2B, $E2, $65   ; $6EF0
    db   $6E, $FE, $06, $66, $6D, $6E, $65, $38   ; $6EF8
    db   $55, $E2, $66, $6D, $FE, $09, $65, $6E   ; $6F00
    db   $6D, $6E, $65, $65, $65, $65, $65, $E2   ; $6F08
    db   $65, $6E, $FE, $09, $66, $6D, $6E, $6D   ; $6F10
    db   $66, $66, $66, $66, $66, $E2, $66, $6D   ; $6F18
    db   $FE, $04, $65, $6E, $6D, $66, $51, $57   ; $6F20
    db   $E2, $65, $4E, $FE, $03, $66, $6D, $6E   ; $6F28
    db   $51, $55, $B5, $70, $72, $72, $6E, $6D   ; $6F30
    db   $FE, $03, $65, $6E, $6D, $51, $55, $B5   ; $6F38
    db   $71, $73, $73, $6D, $6E, $FE, $03, $66   ; $6F40
    db   $6D, $6E, $51, $58, $E2, $66, $6D, $FE   ; $6F48
    db   $03, $65, $6E, $6D, $43, $2B, $2B, $56   ; $6F50
    db   $E2, $65, $6E, $FE, $03, $66, $6D, $6E   ; $6F58
    db   $51, $25, $E2, $66, $6D, $FE, $03, $65   ; $6F60
    db   $6E, $6D, $51, $57, $A6, $70, $72, $72   ; $6F68
    db   $72, $72, $72, $FE, $03, $66, $6D, $6E   ; $6F70
    db   $A6, $71, $73, $73, $73, $73, $73, $FE   ; $6F78
    db   $03, $65, $6E, $6D, $FE, $03, $66, $6D   ; $6F80
    db   $6E, $FE, $03, $65, $6E, $6D, $97, $70   ; $6F88
    db   $72, $72, $72, $72, $72, $72, $FE, $03   ; $6F90
    db   $66, $6D, $6E, $97, $71, $73, $73, $73   ; $6F98
    db   $73, $73, $73, $FE, $03, $65, $6E, $6D   ; $6FA0
    db   $FE, $03, $66, $6D, $6E, $51, $57, $FE   ; $6FA8
    db   $03, $65, $6E, $6D, $51, $58, $E1, $65   ; $6FB0
    db   $FE, $03, $66, $6D, $6E, $A6, $70, $72   ; $6FB8
    db   $72, $72, $6E, $65, $FE, $03, $65, $6E   ; $6FC0
    db   $6D, $51, $55, $A6, $71, $73, $73, $73   ; $6FC8
    db   $6D, $6E, $FE, $03, $66, $6D, $6E, $51   ; $6FD0
    db   $56, $E2, $66, $6D, $FE, $03, $65, $6E   ; $6FD8
    db   $6D, $51, $57, $E2, $65, $6E, $FE, $04   ; $6FE0
    db   $66, $6D, $6E, $65, $51, $58, $E2, $66   ; $6FE8
    db   $6D, $FE, $06, $65, $6E, $6D, $6E, $6D   ; $6FF0
    db   $56, $C4, $65, $65, $6D, $6E, $FE, $06   ; $6FF8
    db   $66, $6D, $6E, $6D, $6E, $58, $C4, $66   ; $7000
    db   $6E, $6E, $6D, $FE, $05, $65, $6E, $6D   ; $7008
    db   $6E, $6D, $62, $5D, $35, $C4, $65, $6D   ; $7010
    db   $6D, $6E, $FE, $06, $66, $6D, $6E, $6D   ; $7018
    db   $6E, $56, $C4, $66, $6E, $6E, $6D, $FE   ; $7020
    db   $16, $6E, $6D, $6E, $6D, $59, $5B, $C4   ; $7028
    db   $65, $6D, $6D, $6E, $FE, $07, $F4, $6D   ; $7030
    db   $6E, $6D, $6E, $5A, $5C, $C4, $66, $6E   ; $7038
    db   $6E, $6D, $FE, $06, $F4, $6E, $6D, $6E   ; $7040
    db   $6D, $2B, $C4, $65, $6D, $6D, $6E, $FE   ; $7048
    db   $06, $F4, $6D, $6E, $6D, $6E, $29, $C4   ; $7050
    db   $66, $6E, $6E, $6D, $FE, $07, $F4, $6E   ; $7058
    db   $6D, $6E, $6D, $55, $38, $C4, $65, $6D   ; $7060
    db   $6D, $6E, $FE, $07, $F4, $6D, $6E, $6D   ; $7068
    db   $6E, $57, $38, $C4, $66, $6E, $6E, $6D   ; $7070
    db   $FE, $07, $F4, $6E, $6D, $6E, $6D, $55   ; $7078
    db   $56, $C4, $65, $6D, $6D, $6E, $FE, $06   ; $7080
    db   $F4, $65, $6E, $6D, $6E, $55, $C4, $66   ; $7088
    db   $6E, $6E, $6D, $FE, $06, $F4, $66, $6D   ; $7090
    db   $6E, $6D, $57, $C4, $70, $72, $6D, $6E   ; $7098
    db   $FE, $06, $F4, $65, $6E, $6D, $6E, $2B   ; $70A0
    db   $C4, $71, $73, $6E, $6D, $FE, $05, $F4   ; $70A8
    db   $66, $6D, $6E, $6D, $C4, $65, $6D, $6D   ; $70B0
    db   $6E, $FE, $05, $F4, $65, $6E, $6D, $6E   ; $70B8
    db   $62, $5D, $35, $C4, $66, $6E, $6E, $6D   ; $70C0
    db   $FE, $06, $F4, $66, $6D, $6E, $6D, $55   ; $70C8
    db   $C4, $65, $6D, $6D, $6E, $FE, $06, $F4   ; $70D0
    db   $65, $6E, $6D, $6E, $58, $C4, $66, $6E   ; $70D8
    db   $6E, $6D, $FE, $07, $F4, $66, $6D, $6E   ; $70E0
    db   $6D, $56, $38, $C4, $70, $72, $6D, $6E   ; $70E8
    db   $FE, $15, $65, $6E, $6D, $6E, $55, $C4   ; $70F0
    db   $71, $73, $6E, $6D, $FE, $11, $66, $31   ; $70F8
    db   $66, $A6, $70, $72, $72, $72, $6D, $6E   ; $7100
    db   $FE, $22, $59, $5B, $A6, $71, $73, $73   ; $7108
    db   $73, $6E, $6D, $FE, $22, $5A, $5C, $E2   ; $7110
    db   $65, $6E, $FE, $E2, $66, $6D, $FE, $31   ; $7118
    db   $38, $E2, $65, $6E, $FE, $31, $55, $52   ; $7120
    db   $5D, $35, $E2, $66, $6D, $FE, $32, $56   ; $7128
    db   $55, $E2, $65, $66, $FE, $32, $57, $2B   ; $7130
    db   $E1, $66, $FE, $32, $58, $2B, $FE, $32   ; $7138
    db   $57, $38, $FE, $32, $56, $38, $FE, $FE   ; $7140
    db   $31, $56, $52, $5D, $35, $E1, $65, $FE   ; $7148
    db   $31, $55, $E2, $66, $65, $FE, $31, $56   ; $7150
    db   $E2, $65, $6E, $FE, $31, $57, $E2, $66   ; $7158
    db   $6D, $FE, $23, $59, $5B, $38, $E2, $65   ; $7160
    db   $6E, $FE, $14, $65, $5A, $5C, $55, $E2   ; $7168
    db   $66, $4D, $FE, $02, $65, $6E, $42, $FD   ; $7170
    db   $38, $E2, $65, $4E, $FE, $02, $66, $6D   ; $7178
    db   $32, $65, $56, $E2, $66, $4D, $FE, $04   ; $7180
    db   $64, $64, $51, $55, $B5, $FD, $64, $FE   ; $7188
    db   $04, $64, $64, $51, $56, $B5, $FD, $64   ; $7190
    db   $FE, $04, $64, $64, $51, $57, $E2, $3B   ; $7198
    db   $53, $FE, $06, $64, $64, $51, $55, $5D   ; $71A0
    db   $35, $E2, $3B, $53, $FE, $03, $64, $64   ; $71A8
    db   $51, $E2, $FD, $64, $FE, $05, $64, $64   ; $71B0
    db   $51, $59, $5B, $E2, $FD, $64, $FE, $05   ; $71B8
    db   $64, $64, $51, $5A, $5C, $E2, $FD, $64   ; $71C0
    db   $FE, $05, $64, $64, $51, $55, $56, $E2   ; $71C8
    db   $FD, $64, $FE, $04, $64, $64, $51, $2B   ; $71D0
    db   $E2, $3B, $53, $FE, $04, $64, $64, $51   ; $71D8
    db   $55, $D3, $7F, $3B, $53, $FE, $05, $64   ; $71E0
    db   $64, $51, $57, $56, $D3, $7F, $3B, $53   ; $71E8
    db   $FE, $05, $64, $64, $51, $59, $5B, $D3   ; $71F0
    db   $7F, $3B, $53, $FE, $05, $64, $64, $51   ; $71F8
    db   $5A, $5C, $D3, $7F, $3B, $53, $FE, $03   ; $7200
    db   $64, $64, $51, $D3, $7F, $3B, $53, $FE   ; $7208
    db   $06, $64, $64, $3D, $52, $5D, $35, $C4   ; $7210
    db   $3A, $7F, $3B, $53, $FE, $06, $64, $64   ; $7218
    db   $64, $64, $51, $56, $C4, $FD, $64, $FE   ; $7220
    db   $06, $64, $64, $64, $64, $51, $2B, $C4   ; $7228
    db   $FD, $64, $FE, $06, $64, $64, $64, $64   ; $7230
    db   $51, $57, $C4, $FD, $64, $FE, $04, $64   ; $7238
    db   $64, $4F, $50, $51, $55, $C4, $FD, $64   ; $7240
    db   $FE, $03, $64, $64, $51, $51, $58, $C4   ; $7248
    db   $FD, $64, $FE, $13, $66, $6D, $6E, $F1   ; $7250
    db   $66, $FE, $22, $66, $6D, $FE, $22, $65   ; $7258
    db   $6E, $FE, $22, $66, $6D, $FE, $22, $65   ; $7260
    db   $6E, $52, $5D, $35, $E1, $65, $FE, $22   ; $7268
    db   $66, $6D, $E2, $66, $65, $FE, $22, $65   ; $7270
    db   $6E, $E2, $65, $6E, $FE, $22, $66, $6D   ; $7278
    db   $E2, $66, $6D, $FE, $13, $70, $72, $6E   ; $7280
    db   $E2, $65, $6E, $FE, $13, $71, $73, $6D   ; $7288
    db   $D3, $63, $6E, $6D, $FE, $31, $66, $C4   ; $7290
    db   $63, $63, $6D, $6E, $FE, $B5, $63, $63   ; $7298
    db   $63, $6E, $6D, $FE, $A6, $63, $63, $63   ; $72A0
    db   $63, $6D, $6E, $FE, $13, $59, $5B, $55   ; $72A8
    db   $97, $63, $63, $63, $63, $63, $6E, $6D   ; $72B0
    db   $FE, $13, $5A, $5C, $56, $A6, $63, $63   ; $72B8
    db   $63, $63, $6D, $6E, $FE, $31, $57, $B5   ; $72C0
    db   $63, $63, $63, $6E, $6D, $FE, $11, $2B   ; $72C8
    db   $31, $58, $C4, $63, $63, $6D, $6E, $FE   ; $72D0
    db   $11, $56, $52, $5D, $35, $D3, $63, $6E   ; $72D8
    db   $6D, $FE, $11, $55, $31, $56, $E2, $65   ; $72E0
    db   $6E, $FE, $11, $57, $31, $55, $E2, $66   ; $72E8
    db   $64, $FE, $F1, $64, $FE, $F1, $64, $FE   ; $72F0
    db   $D1, $7F, $F1, $64, $FE, $01, $65, $21   ; $72F8
    db   $65, $41, $65, $65, $65, $81, $82, $82   ; $7300
    db   $82, $D1, $7F, $F1, $64, $FE, $0B, $66   ; $7308
    db   $65, $6E, $65, $66, $65, $66, $81, $82   ; $7310
    db   $82, $82, $D1, $7F, $F1, $64, $FE, $04   ; $7318
    db   $65, $6E, $6D, $66, $51, $66, $B1, $6B   ; $7320
    db   $D1, $6B, $F1, $64, $FE, $04, $66, $6D   ; $7328
    db   $66, $55, $A2, $F4, $6B, $D1, $6B, $F1   ; $7330
    db   $64, $FE, $02, $65, $6E, $42, $5D, $35   ; $7338
    db   $B3, $6B, $F4, $6B, $F1, $64, $FE, $02   ; $7340
    db   $66, $6D, $32, $59, $5B, $A2, $F4, $6B   ; $7348
    db   $D1, $6B, $F1, $64, $FE, $02, $65, $6E   ; $7350
    db   $32, $5A, $5C, $B3, $6B, $F4, $6B, $F1   ; $7358
    db   $64, $FE, $02, $66, $6D, $31, $58, $A2   ; $7360
    db   $F4, $6B, $D1, $6B, $F1, $64, $FE, $02   ; $7368
    db   $65, $6E, $31, $56, $77, $52, $52, $52   ; $7370
    db   $52, $6B, $F4, $6B, $F1, $64, $FE, $02   ; $7378
    db   $66, $6D, $31, $57, $75, $64, $64, $64   ; $7380
    db   $64, $6B, $D1, $6B, $F1, $64, $FE, $02   ; $7388
    db   $65, $6E, $31, $58, $72, $64, $5F, $D1   ; $7390
    db   $6B, $F1, $64, $FE, $02, $66, $6D, $71   ; $7398
    db   $64, $D1, $6B, $F1, $64, $FE, $02, $65   ; $73A0
    db   $6E, $31, $55, $71, $64, $D1, $6B, $F1   ; $73A8
    db   $64, $FE, $02, $66, $6D, $31, $57, $71   ; $73B0
    db   $64, $D1, $6B, $F1, $64, $FE, $02, $65   ; $73B8
    db   $6E, $31, $56, $71, $50, $D1, $6B, $F1   ; $73C0
    db   $64, $FE, $02, $66, $6D, $31, $57, $D1   ; $73C8
    db   $6B, $F1, $64, $FE, $03, $65, $6E, $65   ; $73D0
    db   $42, $5D, $35, $D1, $6B, $F1, $64, $FE   ; $73D8
    db   $04, $66, $6D, $6E, $57, $C2, $63, $6B   ; $73E0
    db   $F1, $64, $FE, $04, $65, $6E, $6D, $55   ; $73E8
    db   $B3, $63, $F4, $F4, $F1, $64, $FE, $03   ; $73F0
    db   $66, $6D, $66, $A6, $63, $F4, $F4, $F4   ; $73F8
    db   $52, $64, $FE, $02, $65, $6E, $97, $63   ; $7400
    db   $F4, $F4, $F4, $F4, $64, $64, $FE, $02   ; $7408
    db   $66, $6D, $81, $63, $A6, $F4, $F4, $F4   ; $7410
    db   $F4, $50, $64, $FE, $02, $65, $6E, $71   ; $7418
    db   $63, $A4, $FD, $F4, $F1, $64, $FE, $02   ; $7420
    db   $66, $6D, $31, $56, $61, $63, $A4, $F4   ; $7428
    db   $81, $F4, $F4, $F1, $64, $FE, $02, $65   ; $7430
    db   $6E, $31, $55, $71, $63, $A4, $FD, $F4   ; $7438
    db   $F1, $64, $FE, $02, $66, $6D, $81, $63   ; $7440
    db   $A4, $FD, $F4, $F1, $64, $FE, $02, $65   ; $7448
    db   $6E, $31, $56, $95, $63, $F4, $F4, $F4   ; $7450
    db   $F4, $F1, $64, $FE, $02, $66, $6D, $32   ; $7458
    db   $57, $2B, $A4, $82, $F4, $F4, $F4, $F1   ; $7460
    db   $64, $FE, $02, $65, $6E, $32, $55, $2B   ; $7468
    db   $B3, $82, $F4, $F4, $F1, $64, $FE, $02   ; $7470
    db   $66, $6D, $32, $59, $5B, $C2, $63, $F4   ; $7478
    db   $F1, $64, $FE, $02, $65, $6E, $32, $5A   ; $7480
    db   $5C, $D1, $63, $FE, $02, $66, $6D, $31   ; $7488
    db   $56, $D3, $FD, $64, $FE, $02, $65, $6E   ; $7490
    db   $33, $57, $5D, $35, $D3, $FD, $64, $FE   ; $7498
    db   $02, $66, $6D, $31, $55, $A6, $52, $52   ; $74A0
    db   $52, $64, $64, $64, $FE, $02, $64, $6E   ; $74A8
    db   $A6, $FD, $64, $FE, $05, $64, $64, $51   ; $74B0
    db   $59, $5B, $A6, $FD, $64, $FE, $05, $64   ; $74B8
    db   $64, $51, $5A, $5C, $A6, $FD, $64, $FE   ; $74C0
    db   $04, $64, $64, $51, $2B, $A6, $FD, $50   ; $74C8
    db   $FE, $04, $64, $64, $51, $57, $97, $FD   ; $74D0
    db   $52, $FE, $04, $64, $64, $51, $25, $97   ; $74D8
    db   $FD, $64, $FE, $03, $64, $64, $51, $42   ; $74E0
    db   $5D, $35, $97, $FD, $64, $FE, $03, $64   ; $74E8
    db   $64, $51, $97, $50, $50, $64, $64, $64   ; $74F0
    db   $64, $64, $FE, $05, $64, $64, $3D, $52   ; $74F8
    db   $52, $B5, $FD, $64, $FE, $06, $64, $64   ; $7500
    db   $64, $64, $64, $51, $72, $FD, $82, $B5   ; $7508
    db   $FD, $64, $FE, $06, $64, $64, $64, $64   ; $7510
    db   $64, $51, $79, $82, $82, $48, $4A, $64   ; $7518
    db   $64, $64, $64, $64, $FE, $06, $64, $64   ; $7520
    db   $64, $64, $64, $51, $79, $82, $82, $49   ; $7528
    db   $4B, $64, $64, $64, $64, $64, $FE, $06   ; $7530
    db   $64, $64, $64, $64, $64, $51, $72, $FD   ; $7538
    db   $82, $A6, $4C, $64, $64, $64, $64, $64   ; $7540
    db   $FE, $06, $64, $64, $64, $64, $64, $51   ; $7548
    db   $72, $FD, $82, $B5, $FD, $64, $FE, $06   ; $7550
    db   $64, $64, $64, $64, $64, $51, $79, $80   ; $7558
    db   $82, $48, $4A, $64, $64, $64, $64, $64   ; $7560
    db   $FE, $06, $64, $64, $64, $64, $64, $51   ; $7568
    db   $79, $82, $82, $49, $4B, $64, $64, $64   ; $7570
    db   $64, $64, $FE, $06, $64, $64, $64, $64   ; $7578
    db   $64, $51, $72, $FD, $82, $A6, $4C, $64   ; $7580
    db   $64, $64, $64, $64, $FE, $05, $64, $64   ; $7588
    db   $4F, $50, $50, $B5, $FD, $64, $FE, $06   ; $7590
    db   $64, $64, $51, $58, $5D, $35, $B5, $FD   ; $7598
    db   $64, $FE, $05, $64, $64, $51, $59, $5B   ; $75A0
    db   $B5, $FD, $50, $FE, $05, $64, $64, $51   ; $75A8
    db   $5A, $5C, $FE, $09, $64, $64, $51, $55   ; $75B0
    db   $56, $57, $55, $56, $55, $FE, $03, $64   ; $75B8
    db   $64, $51, $B5, $FD, $52, $FE, $03, $64   ; $75C0
    db   $64, $51, $51, $55, $C4, $FD, $64, $FE   ; $75C8
    db   $04, $64, $64, $3D, $52, $51, $56, $C4   ; $75D0
    db   $FD, $64, $FE, $06, $64, $64, $64, $64   ; $75D8
    db   $51, $57, $C1, $7F, $E2, $3B, $53, $FE   ; $75E0
    db   $06, $64, $64, $64, $64, $51, $58, $C1   ; $75E8
    db   $7F, $E2, $3B, $53, $FE, $06, $64, $64   ; $75F0
    db   $64, $64, $51, $57, $C1, $7F, $E2, $3B   ; $75F8
    db   $53, $FE, $06, $64, $64, $64, $64, $51   ; $7600
    db   $2B, $C1, $7F, $E2, $3B, $53, $FE, $06   ; $7608
    db   $64, $64, $64, $64, $51, $55, $C1, $7F   ; $7610
    db   $E2, $3B, $53, $FE, $06, $64, $64, $64   ; $7618
    db   $64, $51, $56, $C1, $7F, $E2, $3B, $53   ; $7620
    db   $FE, $06, $64, $64, $64, $64, $51, $57   ; $7628
    db   $C1, $7F, $E2, $3B, $53, $FE, $06, $64   ; $7630
    db   $64, $64, $64, $51, $25, $C1, $7F, $E2   ; $7638
    db   $3B, $53, $FE, $06, $64, $64, $64, $64   ; $7640
    db   $51, $56, $C1, $7F, $E2, $3B, $53, $FE   ; $7648
    db   $06, $64, $64, $64, $64, $51, $55, $C1   ; $7650
    db   $7F, $E2, $3B, $53, $FE, $06, $64, $64   ; $7658
    db   $64, $64, $51, $56, $C1, $7F, $E2, $3B   ; $7660
    db   $53, $FE, $05, $64, $64, $64, $64, $51   ; $7668
    db   $C1, $7F, $E2, $3B, $53, $FE, $07, $64   ; $7670
    db   $64, $64, $64, $51, $5D, $35, $C1, $7F   ; $7678
    db   $E2, $3B, $53, $FE, $05, $64, $64, $64   ; $7680
    db   $64, $51, $B2, $3A, $7F, $E2, $3B, $53   ; $7688
    db   $FE, $07, $64, $64, $64, $64, $51, $5D   ; $7690
    db   $35, $A1, $3A, $C1, $7F, $E2, $3B, $53   ; $7698
    db   $FE, $06, $64, $64, $64, $64, $3D, $52   ; $76A0
    db   $97, $E1, $64, $64, $64, $64, $64, $64   ; $76A8
    db   $FE, $00, $64, $64, $64, $64, $64, $64   ; $76B0
    db   $EC, $EC, $EC, $EC, $64, $64, $64, $64   ; $76B8
    db   $64, $64, $FE, $06, $FD, $64, $A6, $FD   ; $76C0
    db   $64, $FE, $00, $FD, $7F, $FE, $E2, $FD   ; $76C8
    db   $7F, $FE, $E2, $FD, $7F, $FE, $E2, $FD   ; $76D0
    db   $7F, $FE, $E2, $FD, $7F, $FE, $01, $7F   ; $76D8
    db   $38, $FD, $82, $E2, $FD, $7F, $FE, $01   ; $76E0
    db   $7F, $A1, $82, $D3, $F4, $7F, $7F, $FE   ; $76E8
    db   $01, $7F, $31, $F4, $51, $F4, $71, $F4   ; $76F0
    db   $92, $F4, $82, $E2, $FD, $7F, $FE, $01   ; $76F8
    db   $7F, $31, $F4, $51, $F4, $71, $F4, $92   ; $7700
    db   $F4, $82, $D3, $F4, $7F, $7F, $FE, $01   ; $7708
    db   $7F, $31, $F4, $51, $F4, $71, $F4, $92   ; $7710
    db   $F4, $82, $E2, $FD, $7F, $FE, $01, $7F   ; $7718
    db   $31, $F4, $51, $F4, $71, $F4, $92, $F4   ; $7720
    db   $82, $D3, $F4, $7F, $7F, $FE, $01, $7F   ; $7728
    db   $31, $F4, $51, $F4, $71, $F4, $92, $F4   ; $7730
    db   $82, $E2, $FD, $7F, $FE, $01, $7F, $31   ; $7738
    db   $F4, $51, $F4, $71, $F4, $92, $F4, $82   ; $7740
    db   $D3, $F4, $7F, $7F, $FE, $01, $7F, $A1   ; $7748
    db   $82, $E2, $FD, $7F, $FE, $01, $7F, $38   ; $7750
    db   $80, $82, $82, $82, $82, $82, $82, $82   ; $7758
    db   $D3, $F4, $7F, $7F, $FE, $E2, $FD, $7F   ; $7760
    db   $FE, $72, $FD, $F4, $C4, $74, $77, $7F   ; $7768
    db   $7F, $FE, $72, $FD, $F4, $C4, $75, $78   ; $7770
    db   $7F, $7F, $FE, $00, $72, $72, $72, $72   ; $7778
    db   $72, $72, $72, $72, $72, $72, $72, $72   ; $7780
    db   $76, $79, $7F, $7F, $FE, $00, $73, $73   ; $7788
    db   $73, $73, $73, $73, $73, $73, $73, $73   ; $7790
    db   $73, $73, $73, $73, $7F, $7F, $FE, $00   ; $7798
    db   $FD, $7F, $FE, $F1, $7F, $FE, $F1, $7F   ; $77A0
    db   $FE, $0C, $FD, $7F, $F1, $7F, $FE, $01   ; $77A8
    db   $7F, $31, $7F, $A2, $F4, $7F, $F1, $7F   ; $77B0
    db   $FE, $01, $7F, $22, $F4, $7F, $62, $F4   ; $77B8
    db   $7F, $B1, $82, $F1, $7F, $FE, $01, $7F   ; $77C0
    db   $31, $80, $71, $82, $A2, $F4, $7F, $F1   ; $77C8
    db   $7F, $FE, $01, $7F, $22, $F4, $7F, $62   ; $77D0
    db   $F4, $7F, $B1, $82, $F1, $7F, $FE, $01   ; $77D8
    db   $7F, $31, $82, $71, $80, $A2, $F4, $7F   ; $77E0
    db   $F1, $7F, $FE, $01, $7F, $22, $F4, $7F   ; $77E8
    db   $62, $F4, $7F, $B1, $82, $F1, $7F, $FE   ; $77F0
    db   $01, $7F, $31, $82, $71, $7F, $A2, $F4   ; $77F8
    db   $7F, $F1, $7F, $FE, $01, $7F, $22, $F4   ; $7800
    db   $7F, $62, $F4, $7F, $B1, $82, $F1, $7F   ; $7808
    db   $FE, $01, $7F, $71, $7F, $A2, $F4, $7F   ; $7810
    db   $F1, $7F, $FE, $01, $7F, $62, $F4, $82   ; $7818
    db   $B1, $82, $F1, $7F, $FE, $01, $7F, $31   ; $7820
    db   $7F, $71, $7F, $A2, $F4, $7F, $F1, $7F   ; $7828
    db   $FE, $01, $7F, $31, $7F, $62, $F4, $7F   ; $7830
    db   $B1, $80, $F1, $7F, $FE, $04, $7F, $74   ; $7838
    db   $77, $7F, $71, $7F, $A2, $F4, $7F, $F1   ; $7840
    db   $7F, $FE, $04, $7F, $75, $78, $7F, $62   ; $7848
    db   $F4, $7F, $B1, $82, $F1, $7F, $FE, $04   ; $7850
    db   $72, $76, $79, $7F, $71, $7F, $F1, $7F   ; $7858
    db   $FE, $00, $73, $73, $73, $7F, $7F, $7F   ; $7860
    db   $7F, $7F, $7F, $7F, $7F, $7F, $7F, $7F   ; $7868
    db   $7F, $7F, $FE, $00, $00, $00, $00, $00   ; $7870
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $7878
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $7880
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $7888
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $7890
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $7898
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78A0
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78A8
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78B0
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78B8
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78C0
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78C8
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78D0
    db   $02, $00, $00, $00, $00, $00, $00, $00   ; $78D8
    db   $00, $00, $00, $00, $08, $00, $00, $00   ; $78E0
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78E8
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $78F0
    db   $00, $00, $00, $00, $00, $04, $00, $00   ; $78F8
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $7900
    db   $00, $00, $00, $00, $00, $00, $00, $00   ; $7908
    db   $00, $00, $00, $00, $00, $10, $00, $00   ; $7910
    db   $00, $00                                 ; $7918

MenuTiles1::
INCBIN "gfx/menuTiles1.2bpp"
MenuTiles1End:

MenuTiles2::
INCBIN "gfx/menuTiles2.2bpp"
MenuTiles2End:
