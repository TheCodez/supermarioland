INCLUDE "charmap.asm"
INCLUDE "constants.asm"
INCLUDE "gbhw.asm"
INCLUDE "vram.asm"
INCLUDE "wram.asm"
INCLUDE "hram.asm"
INCLUDE "macros.asm"
INCLUDE "enemies.asm"

SECTION "bank0", ROM0[$0000]

; Unused
SECTION "RST 0", ROM0[$0000]
	jp Init

; Unused
SECTION "RST 8", ROM0[$0008]
	jp Init

SECTION "RST 28", ROM0[$0028]
; Immediately following the return address is a jump table
; A is used as index
TableJump::
	add a		; Multiply A by 2, as addresses are 16 bit
	pop hl
	ld e, a
	ld d, $00
	add hl, de	; Add the offset to the base address
	ld e, [hl]	; Load the address at that offset into DE
	inc hl
	ld d, [hl]
	push de		; Jump to the target address
	pop hl
	jp hl

; Interrupts
SECTION "Interrupt VBlank", ROM0[$0040]
	jp VBlank

SECTION "Interrupt LCD STAT", ROM0[$0048]
	jp LCDStatus

SECTION "Interrupt Timer", ROM0[$0050]
; Switch to bank 3 (XXX contains music code?)
; Overlaps into serial interrupt mid opcode, but it's unused anyway
	push af
	ld a, BANK(Call_7FF0)
	ld [MBC1RomBank], a
	call Call_7FF0 ; TODO
	ldh a, [hActiveRomBank]
	ld [MBC1RomBank], a
	pop af
	reti

INCLUDE "vblank.asm"

INCLUDE "lcd_stat.asm"

; Missing values will be filled in by rgbfix
SECTION "Header", ROM0[$104]
	ds $30		; Nintendo Logo
				; SUPER MARIOLAND in ASCII
	db $53, $55, $50, $45, $52, $20, $4D, $41, $52, $49, $4F, $4C, $41, $4E, $44
	db 00		; DMG - classic Game Boy
	db 00, 00	; No new licensee code
	db 00		; No SGB functions
	db 01		; MBC1 
	db 01		; 64kB, 4 banks
	db 00		; No RAM
	db 00		; Japanese
	db 01		; Old licensee code: Nintendo
	db 01		; First revision
	ds 1		; Header Checksum
	ds 2		; Global Checksum

INCLUDE "init.asm"

; X and Y coordinates in FFAD and FFAE
LookupTile:: ; 153
	call _LookupTile	; If we're really unlucky, a timer interrupt might
	WAIT_FOR_HBLANK		; fire between the WAIT_FOR_HBLANK, and the actual
	ld b, [hl]			; load into B. If that interrupt takes too long, the
	WAIT_FOR_HBLANK		; PPU can go into mode 3, where reading from VRAM is 
	ld a, [hl]			; impossible, and $FF is returned. So the tile number is
	and b				; read twice, and ANDed together. Without this, Mario
	ret					; very occasionally bumps into an invisible solid block

; Add BCD encoded DE to the score. Signal that the displayed version
; needs to be updated
AddScore:: ; 0166
	ldh a, [$FF9F]		; Demo mode?
	and a
	ret nz
	ld a, e
	ld hl, wScore
	add [hl]			; Add E to the ones and tens
	daa
	ldi [hl], a
	ld a, d				; Add D to the hundreds and thousands
	adc [hl]			; with carry from the lower digits
	daa
	ldi [hl], a
	ld a, 0				; No score above 9999 is ever awarded, so just add
	adc [hl]			; 0 to propagate carry
	daa
	ld [hl], a
	ld a, 1
	ldh [$FFB1], a		; TODO We've seen this address before
	ret nc
	ld a, $99			; Score saturates at 999999
	ldd [hl], a
	ldd [hl], a
	ld [hl], a
	ret

Init::	; 0185
	ld a, (1 << VBLANK) | (1 << LCD_STAT)
	di
	ldh [rIF], a
	ldh [rIE], a
	ld a, $40
	ldh [rSTAT], a
	xor a
	ldh [rSCY], a
	ldh [rSCX], a
	ldh [$A4], a
	ld a, %10000000
	ldh [rLCDC], a	; Turn LCD on, but don't display anything
.waitVBlank
	ldh a, [rLY]
	cp a, SCRN_Y + 4 ; $94
	jr nz, .waitVBlank	; Waits for VBlank?
	
	ld a, %00000011
	ldh [rLCDC], a	; Turn LCD off
	ld a, $E4
	ldh [rBGP], a
	ldh [rOBP0], a
	ld a, $54
	ldh [rOBP1], a
	ld hl, rNR52
	ld a, $80
	ldd [hl], a		; Turn the sound on
	ld a, $FF
	ldd [hl], a		; Output all sounds to both terminals
	ld [hl], $77	; Turn the volume up to the max
	ld sp, $CFFF

	xor a
	ld hl, $DFFF	; End of Work RAM
	ld c, $40
	ld b, 0			; CB = $4000, size of Work RAM
.clearWRAMloop		; Also clears non-existent cartridge RAM. Bug?
	ldd [hl], a
	dec b
	jr nz, .clearWRAMloop
	dec c			; Why this doesn't use a loop on BC is beyond me...
	jr nz, .clearWRAMloop

	ld hl, $9FFF	; End of Video RAM
	ld c, $20		; CB = $2000, size of Video RAM
	xor a			; Unnecessary
	ld b, 0
.clearVRAMloop
	ldd [hl], a
	dec b
	jr nz, .clearVRAMloop
	dec c
	jr nz, .clearVRAMloop

	ld hl, $FEFF	; End of OAM (well, over the end, bug?)
	ld b, 0			; Underflow, clear $FF bytes
.clearOAMloop
	ldd [hl], a
	dec b
	jr nz, .clearOAMloop

	ld hl, $FFFE	; End of High RAM
	ld b, $80		; Size of High RAM, off by one, bug?
.clearHRAMloop
	ldd [hl], a
	dec b
	jr nz, .clearHRAMloop

	ld c, LOW(hDMARoutine)
	ld b, DMARoutineEnd - DMARoutine + 2 ; Bug
	ld hl, DMARoutine
.copyDMAroutine		; No memory can be accessed during DMA other than HRAM,
	ldi a, [hl]		; so the routine is copied and executed there
	ld [$FF00+c], a
	inc c
	dec b
	jr nz, .copyDMAroutine

	xor a
	ldh [hLevelIndex], a
	ld a, $11
	ldh [hWorldAndLevel], a
	ld [wContinueWorldAndLevel], a
	ld a, 2
	ld [$C0DC], a
	ld a, STATE_LOAD_MENU
	ldh [hGameState], a
	ld a, BANK(InitSound) ; 3
	ld [MBC1RomBank], a
	ld [$C0A4], a
	ld a, 0
	ld [wWinCount], a
	ldh [hWinCount], a
	call InitSound
	ld a, 2
	ld [MBC1RomBank], a
	ldh [hActiveRomBank], a

.mainLoop
	ld a, [wGameTimerExpiringFlag]
	cp a, 3					; 0 in normal play, 1 if time < 100
	jr nz, .timeNotUp		; 2 if time < 50, 3 if time = 0, FF if time up
	ld a, $FF
	ld [wGameTimerExpiringFlag], a
	call KillMario			; die when timer expired
	call Call_1736
.timeNotUp
	homecall ReadJoypad
	ldh a, [$FF9F]			; Demo mode?
	and a
	jr nz, .decrementTimers	; Can't pause the demo
	call pauseOrReset
	ldh a, [hGamePaused]
	and a
	jr nz, .halt
.decrementTimers
	ld hl, hTimer
	ld b, 2
.nextTimer
	ld a, [hl]
	and a
	jr z, .skip			; don't decrement below 0
	dec [hl]			; FFA{6,7} are generic frame based timers
.skip
	inc l
	dec b
	jr nz, .nextTimer
	ldh a, [$FF9F]		; Equal to 28 in menu and during demo
	and a
	jr z, .run
	ldh a, [hJoyHeld]
	bit START_BIT, a			; test for Start
	jr nz, .startGame
	ldh a, [hFrameCounter] ; todo
	and a, $0F
	jr nz, .run
	ld hl, $C0D7
	ld a, [hl]
	and a
	jr z, .startGame
	dec [hl]
	jr .run

.startGame
	ldh a, [hGameState]	; 0 corresponds to normal gameplay...
	and a
	jr nz, .run			; start the game?
	ld a, 2
	ld [MBC1RomBank], a
	ldh [hActiveRomBank], a
	ld a, STATE_LOAD_MENU
	ldh [hGameState], a	; go back from demo to menu
.run
	call HandleGameState		; This will the return address for the imminent rst $28
.halt
	halt						; Halt the CPU until an interrupt is fired
	ldh a, [hVBlankOccurred]
	and a
	jr z, .halt
	xor a
	ldh [hVBlankOccurred], a
	jr .mainLoop

; keep looping. never reached
.loop
	jr .loop

HandleGameState::
	ldh a, [hGameState]
	rst $28		; Jump Table
	dw HandleGamePlay ; 0x00   Normal gameplay
	dw HandleDeath ; 0x01 ✓ Dead?
	dw ResetToCheckpoint ; 0x02 ✓ Reset to checkpoint
	dw PrepareDeath ; 0x03 ✓ Pre dying
	dw HandleDying ; 0x04 ✓ Dying animation
	dw HandleLevelEndCountdownTimer  ; 0x05 ✓ Explosion/Score counting down
	dw HandleLevelEnd ; 0x06 ✓ End of level
	dw HandleLevelEndFX ; 0x07 ✓ End of level gate, music
	dw HandlePrepareNextLevel.entryPoint ; 0x08 ✓ Increment Level, load tiles
	dw HandleGoingDownPipe ; 0x09 ✓ Going down a pipe
	dw HandleUndergroundWarping ; 0x0A ✓ Warping to underground?
	dw HandleGoingRightIntoPipe ; 0x0B ✓ Going right in a pipe
	dw HandleGoingUpOutOfPipe ; 0x0C ✓ Going up out of a pipe
	dw HandleAutoScrollLevel ; 0x0D   Auto scrolling level
	dw InitMenu ; $0E ✓ Init menu 14
	dw HandleStartMenu.entryPoint ; 0x0F ✓ Start menu
	dw UnusedState ; 0x10 ✓ Unused? todo
	dw HandleStartLevel ; 0x11 ✓ Level start
	dw HandleGotoBonusGame ; 0x12 ✓ Go to Bonus game
	dw HandleEnterBonusGame ; 0x13 ✓ Entering Bonus game
	dw HandleBonusGameMarioSprites ; 0x14 ✓ Setup Mario sprite
	dw HandleBonusGame ; 0x15 ✓ Bonus game
	dw HandleMoveLadder ; 0x16 ✓ Move the ladder
	dw HandleBonusGameWalking ; 0x17 ✓ Bonus game walking
	dw HandleBonusGameDescendLadder ; 0x18 ✓ Bonus game descending ladder
	dw HandleBonusGameAscendLadder ; 0x19 ✓ Bonus game ascending ladder
	dw GameState_1A ; 0x1A ✓ Getting price
	dw HandleLeaveBonusGame ; 0x1B ✓ Leaving Bonus game
	dw GameState_1C ; 0x1C ✓ Smth with the gate after a boss
	dw GameState_1D ; 0x1D ✓
	dw HandleGateOpening ; 0x1E ✓ Gate opening
	dw HandleGateOpen ; 0x1F ✓ Gate open
	dw MarioStartsWalkingOffscreen ; 0x20 ✓ Walk off button
	dw HandleMarioWalksOffscreen ; 0x21 ✓ Mario offscreen
	dw HandleScrollToDaisy ; 0x22 ✓ Scroll to fake Daisy
	dw HandleWalkToFakeDaisy ; 0x23 ✓ Walk to fake Daisy
	dw HandleFakeDaisySpeaking ; 0x24 ✓ Fake Daisy speak
	dw HandleFakeDaisyMorphing ; 0x25 ✓ Fake Daisy morphing
	dw HandleFakeDaisyJumpingAway ; 0x26 ✓ Fake Daisy monster jumping away
	dw HandleTatangaDying ; 0x27 ✓ Tatanga dying
	dw HandleTatangaDead ; 0x28 ✓ Tatanga dead, plane moves forward
	dw GameState_29 ; 0x29 ✓ 
	dw HandleDaisySpeaking ; 0x2A ✓ Daisy speaking
	dw HandleDaisyMovingTowardsMario ; 0x2B ✓ Daisy moving
	dw HandleDaisyKiss ; 0x2C ✓ Daisy kissing
	dw HandleDaisyQuestOver ; 0x2D ✓ Daisy quest over
	dw HandleMarioAndDaisyWalking ; 0x2E ✓ Mario credits running
	dw HandleEnterAirplane ; $2F ✓ Entering airplane
	dw HandleAirplaneTakingOff ; 0x30 ✓ Airplane taking off
	dw HandleAirplaneMovingForward ; 0x31 ✓ Airplane moving forward
	dw HandleAirplaneLeavingHanger ; 0x32 ✓ Airplane leaving hanger?
	dw GameState_33.entryPoint ; 0x33 ✓ In between two credits?
	dw HandleCreditsComingUp ; $34 ✓ Credits coming up
	dw HandleCreditsStandingStill ; $35 ✓ Credits stand still
	dw HandleCreditsLeave ; $36 ✓ Credits leave
	dw HandleAirplaneLeaving ; $37 ✓ Airplane leaving
	dw HandleTheEnd ; $38 ✓ THE END letters flying
	dw PrepareGameOver ; $39 ✓ Pre game over?
	dw HandleGameOver ; $3A ✓ Game over
	dw PrepareTimesUp ; $3B ✓ Pre time up
	dw HandleTimesUp ; $3C ✓ Time up
	dw HandleGamePlay.return ; $3D  
 
;322
InitMenu::
	xor a
	ldh [rLCDC], a	; Turn off LCD
	di
	ldh [hScrollX], a
	ld hl, wOAMBuffer
	ld b, $9F
.clearSpritesLoop
	ldi [hl], a
	dec b
	jr nz, .clearSpritesLoop
	ldh [hSuperStatus], a
	ld [wGameOverWindowEnabled], a
	ld [wGameOverTimerExpired], a
	ld hl, $C0D8
	ldi [hl], a
	ldi [hl], a
	ldi [hl], a
	ld a, [wWinCount]	; restore from Work RAM, possibly overwritten for demo
	ldh [hWinCount], a	; Expert Mode activated when non zero
	ld hl, MenuTiles1
	ld de, vChars2 + $300 ; $9300
	ld bc, MenuTiles1End - MenuTiles1
	call CopyData	; loads tiles for the menu
	ld hl, MenuTiles2
	ld de, vChars1
	ld bc, MenuTiles2End - MenuTiles2
	call CopyData	; and more tiles
	ld hl, $4862
	ldh a, [hWinCount]
	cp a, 1
	jr c, .noWins	; no win yet
	ld hl, $4E72
.noWins
	ld de, $8AC0
	ld bc, $0010
	call CopyData	; mushroom sprite (or mario head)
	ld hl, $5032
	ld de, vChars2
	ld bc, $02C0
	call CopyData	; font, coins
	ld hl, $5032
	ld de, vChars0
	ld bc, $02A0
	call CopyData	; same, but to the other tile data bank
	call ClearBGMap0
	xor a
	ldh [hScreenIndex], a
	ldh a, [hLevelIndex]
	push af
	ld a, $0C
	ldh [hLevelIndex], a
	call DrawInitialScreen			; Draw level into tile map TODO based on FFE4?
	pop af
	ldh [hLevelIndex], a
	ld a, $3C
	ld hl, vBGMap0		; tile map
	call FillStartMenuTopRow		; usually hidden by the HUD
	ld hl, vBGMap0 + 4 ; $9804
	ld [hl], $94
	ld hl, vBGMap0 + SCRN_VX_B + 2 ; $9822
	ld [hl], $95
	inc l
	ld [hl], $96					; Mario's head
	inc l
	ld [hl], $8C
	ld hl, vBGMap0 + SCRN_VX_B + 15 ; $982F		; Clouds in top right
	ld [hl], $3F
	inc l
	ld [hl], $4C
	inc l
	ld [hl], $4D
	ld hl, wScore + 2
	ld de, wTopScore + 2
	ld b, 3
.compareScores			; Compare the last score to the top score
	ld a, [de]
	sub [hl]
	jr c, .newTopScore
	jr nz, .printTopScore
	dec e
	dec l
	dec b
	jr nz, .compareScores
	jr .printTopScore

.newTopScore			; This is so not the place to do this...
	ld hl, wScore + 2
	ld de, wTopScore + 2
	ld b, 3
.replaceTopScore
	ldd a, [hl]
	ld [de], a
	dec e
	dec b
	jr nz, .replaceTopScore
.printTopScore
	ld de, wTopScore + 2
	ld hl, vBGMap0 + 11 * SCRN_VX_B + 9 ; $9969
	call DisplayScore.fromDEtoHL
	ld hl, wOAMBuffer + 4
	ld [hl], 120			; Y
	ld a, [wNumContinues]
	and a
	jr z, .noContinues
	ldh a, [hWinCount]
	cp a, 2					; after two wins, you don't need 100k points,
	jr c, .printContinues	; you can just always level select
	jr .noContinues

.printContinues
	ld hl, ContinueText
	ld de, vBGMap0 + 14 * SCRN_VX_B + 6 ; $99C6
	ld b, ContinueTextEnd - ContinueText
.loop
	ldi a, [hl]
	ld [de], a
	inc e
	dec b
	jr nz, .loop
	ld hl, wOAMBuffer
	ld [hl], 128	; Y
	inc l
	ld [hl], 136	; X
	inc l
	ld a, [wNumContinues]
	ld [hl], a
	inc l
	ld [hl], 0		; no object attributes
	inc l
	ld [hl], 128	; Y
.noContinues
	inc l
	ld [hl], 40	; X
	inc l
	ld [hl], $AC	; mushroom/mario head
	xor a
	ldh [rIF], a	; Clear all interrupts
	ld a, %11000011
	ldh [rLCDC], a	; Turn on LCD, BG, sprites, and change WIN tile map
	ei
	ld a, STATE_MENU
	ldh [hGameState], a
	xor a
	ldh [hIsUnderground], a
	ld a, $28
	ld [$C0D7], a
	ldh [$FF9F], a
	ld hl, $C0DC
	inc [hl]
	ld a, [hl]
	cp a, 3
	ret nz
	ld [hl], 0
	ret

ContinueText::
	db "continue *"
ContinueTextEnd:

; 450
HandleStartMenu::
.startPressed
	ld a, [wOAMBuffer + 4]
	cp a, 120				; usual start Y position
	jr z, .dontContinue
	ld a, [wNumContinues]	; if not, use up a continue
	dec a
	ld [wNumContinues], a
	ld a, [wContinueWorldAndLevel]
	ldh [hWorldAndLevel], a
	ld e, 0
	cp a, $11				; Convert BCD level to index
	jr z, .startLevelInE
	inc e
	cp a, $12
	jr z, .startLevelInE
	inc e
	cp a, $13
	jr z, .startLevelInE
	inc e
	cp a, $21
	jr z, .startLevelInE
	inc e
	cp a, $22
	jr z, .startLevelInE
	inc e
	cp a, $23
	jr z, .startLevelInE
	inc e
	cp a, $31
	jr z, .startLevelInE
	inc e
	cp a, $32
	jr z, .startLevelInE
	inc e
	cp a, $33
	jr z, .startLevelInE
	inc e
	cp a, $41
	jr z, .startLevelInE
	inc e
	cp a, $42
	jr z, .startLevelInE
	inc e
.startLevelInE
	ld a, e
.storeAndStartLevel
	ldh [hLevelIndex], a
	jp .startLevel

.dontContinue
	xor a
	ld [wNumContinues], a	; harsh
	ldh a, [hWinCount]
	cp a, 2
	jp nc, .startLevel
	ld a, $11
	ldh [hWorldAndLevel], a
	xor a
	jr .storeAndStartLevel

.selectPressed
	ld a, [wNumContinues]
	and a
	jr z, .checkLevelSelect	; if we have no continues, pretend like nothing happened
	ld hl, wOAMBuffer + 4
	ld a, [hl]
	xor a, $F8				; Clever... Switches between 80 and 78
	ld [hl], a
	jr .checkLevelSelect

.entryPoint::
	ldh a, [hJoyPressed]
	ld b, a
	bit START_BIT, b		; START button
	jr nz, .startPressed
	bit SELECT_BIT, b		; SELECT button
	jr nz, .selectPressed
.checkLevelSelect
	ldh a, [hWinCount]
	cp a, 2
	jr c, .checkDemoTimer
	bit A_BUTTON_BIT, b		; A button
	jr z, .drawLevelSelect
	ldh a, [hWorldAndLevel]	; increment the level select
	inc a			; Level
	ld b, a
	and a, $0F
	cp a, 4		; 3 levels per world
	ld a, b
	jr nz, .skip
	add a, $10 - 3	; to increment the upper nibble, the world
.skip
	ldh [hWorldAndLevel], a
	ldh a, [hLevelIndex]
	inc a
	cp a, $0C		; 12 levels in total
	jr nz, .drawNewLevelSelect
	ld a, $11		; go back to 1-1
	ldh [hWorldAndLevel], a
	xor a
.drawNewLevelSelect
	ldh [hLevelIndex], a
.drawLevelSelect	; with sprites
	ld hl, wOAMBuffer + 8 ; $C008
	ldh a, [hWorldAndLevel]	; nibble encoded
	ld b, 120		; Y coordinate
	ld c, a
	and a, $F0		; world
	swap a
	ld [hl], b		; Y
	inc l
	ld [hl], 120	; X for world
	inc l
	ldi [hl], a		; world object index
	inc l
	ld a, c
	and a, $0F
	ld [hl], b		; Y
	inc l
	ld [hl], 136	; X for level
	inc l
	ldi [hl], a		; level object index
	inc l
	ld [hl], b		; Y
	inc l
	ld [hl], 128	; X for -
	inc l
	ld [hl], "-"
.checkDemoTimer
	ld a, [$C0D7]	; Demo timer
	and a
	ret nz
	ld a, [$C0DC]	; Demo select
	sla a
	ld e, a
	ld d, 0
	ld hl, DemoLevels	; Demo levels
	add hl, de
	ldi a, [hl]
	ldh [hWorldAndLevel], a	; pseudo BCD encoded level
	ld a, [hl]
	ldh [hLevelIndex], a	; level index and encoding
	ld a, $50
	ld [$C0D7], a	; timer
	ld a, STATE_START_LEVEL
	ldh [hGameState], a
	xor a
	ldh [hWinCount], a ; to avoid expert mode in demos. Mirrored at C0E1
	ret

.startLevel
	ld a, STATE_START_LEVEL
	ldh [hGameState], a
	xor a
	ldh [rIF], a
	ldh [$FF9F], a
	ld [$C0A4], a
	dec a
	ld [wActiveMusic], a
	ld a, BANK(InitSound)
	ld [MBC1RomBank], a
	call InitSound
	ldh a, [hActiveRomBank]
	ld [MBC1RomBank], a
	xor a
	ld [wSquareSFX], a
	ld [$DFF0], a
	ld [wNoiseSFX], a
	ld a, 7			; enable timer interrupt TODO
	ld [rIE], a
	ret

; more random data... Demo levels: 1-1, 1-2, 3-3
DemoLevels:: ; 569
	db $11, $00, $12, $01, $33, $08

FillStartMenuTopRow: ; 56F
	ld b, SCRN_X_B
.loop
	ldi [hl], a
	dec b
	jr nz, .loop
	ret

HandleStartLevel::	; 576
	xor a
	ldh [rLCDC], a	; turn off LCD
	di
	ldh a, [$FF9F]	; seems to be only non zero in the menu
	and a
	jr nz, .jmp_58B
	xor a
	ld [wScore], a			; ones and tens
	ld [wScore + 1], a		; hundreds and thousands
	ld [wScore + 2], a		; ten and hundred thousands
	ldh [hCoins], a
.jmp_58B
	call PrepareTiles
	call ClearBGMap0
	ld hl, vBGMap1
	ld b, $5F
	ld a, " "
.loop
	ldi [hl], a
	dec b
	jr nz, .loop
	call PrepareHUD
	ld a, 15
	ldh [rLYC], a	; height of the hud?
	ld a, (1 << rTAC_ON) | rTAC_16384_HZ
	ldh [rTAC], a
	ld hl, rWY
	ld [hl], 133
	inc l			; rWX
	ld [hl], 96	; bottom right corner
	xor a
	ldh [rTMA], a
	ldh [rIF], a
	dec a
	ldh [$FFA7], a
	ldh [$FFB1], a
	ld a, $5B
	ldh [$FFE9], a
	call SetIsBackgroundAnimated		; superfluous? happens in HandlePrepareNextLevel.loadWorldTiles?
	call Call_3D1A		; todo
	call DisplayCoins
	call UpdateLives.displayLives
	ldh a, [hWorldAndLevel]
	call HandlePrepareNextLevel.loadWorldTiles

UnusedState:: ; 5CE Unused?
	ret

ClearBGMap0:: ; 05CF
	ld hl, $9BFF
	ld bc, SCRN_VX_B * SCRN_VY_B

EraseTileMap:: ; 5D5
.loop
	ld a, " "
	ldd [hl], a
	dec bc
	ld a, b
	or c
	jr nz, .loop
	ret

INCLUDE "copy.asm"

; 5E7
; prepare tiles
PrepareTiles::	; the three upper banks have tiles at the same location?
	ld hl, $5032
	ld de, vChars2
	ld bc, $0800
	call CopyData
	ld hl, CommonTiles1
	ld de, vChars0
	ld bc, $1000
	call CopyData
	ld hl, $5603
	ld de, $C600	; copy of the animated background tile...
	ld b, 8
.loop
	ldi a, [hl]
	ld [de], a
	inc hl
	inc de
	dec b
	jr nz, .loop
	ret

PrepareHUD::
	ld hl, HUDText
	ld de, vBGMap0
	ld b, 2 ; number of rows
.loop
	ldi a, [hl]
	ld [de], a
	inc e
	ld a, e
	and a, $1F
	cp a, $14
	jr nz, .loop
	ld e, SCRN_VX_B
	dec b
	jr nz, .loop
	ret

; Normal gameplay. Tons of function calls, let's do this later...
HandleGamePlay::	; 627
    call LoadColumns
    call EntityCollision
    ldh a, [hActiveRomBank]
    ldh [hSavedRomBank], a
    ld a, 3
    ldh [hActiveRomBank], a
    ld [MBC1RomBank], a
    call $48FC
    ld bc, wC208
    ld hl, Data_216D
    call Call_490D
    ld bc, $C218
    ld hl, Data_216D
    call Call_490D
    ld bc, $C228
    ld hl, Data_216D
    call Call_490D
    ld bc, $C238
    ld hl, Data_216D
    call Call_490D
    ld bc, $C248
    ld hl, Data_216D
    call Call_490D
    call $4A94
    call $498B
    call $4AEA
    call $4B3C
    call $4B6F
    call $4B8A
    call $4BB5
    ldh a, [hSavedRomBank]
    ldh [hActiveRomBank], a
    ld [MBC1RomBank], a
    call Call_1F2D
    call Call_2491
    ldh a, [hActiveRomBank]
    ldh [hSavedRomBank], a
    ld a, BANK(UpdateTimerAndFloaties)
    ldh [hActiveRomBank], a
    ld [MBC1RomBank], a
    call UpdateTimerAndFloaties
    ldh a, [hSavedRomBank]
    ldh [hActiveRomBank], a
    ld [MBC1RomBank], a
    call Jmp_185D.call_198C
    call Call_16F5
    call Call_17BC
    call Call_AEA
    call Call_A2D
    call Call_1F03
    ld hl, $C0CE
    ld a, [hl]
    and a
    ret z

    dec [hl]
    call Call_2113
.return
    ret

; 06BC
HandleDeath::
	ld hl, hTimer
	ld a, [hl]
	and a
	ret nz
	ld hl, $D100	; enemies
	ld de, $0010
	ld b, $0A
.loop
	ld [hl], $FF
	add hl, de
	dec b
	jr nz, .loop
	xor a
	ldh [hSuperStatus], a
	dec a
	ld [wLivesEarnedLost], a	; FF = -1
	ld a, STATE_RESET_TO_CHECKPOINT
	ldh [hGameState], a
	ret

ResetToCheckpoint::
	di
	ld a, 0
	ldh [rLCDC], a
	call ClearSprites		; clears sprites
	call ClearOverlay		; clears "overlay"
	ld hl, hScreenIndex
	ld a, [hIsUnderground]		; nonzero if underground
	and a
	jr z, .overworld
	xor a
	ldh [hIsUnderground], a		; not underground anymore
	ldh a, [$FFF5]		; screen which we'd've exited out of pipe
	inc a				; to be decremented immediately after
	jr .jmp_6F8

.overworld
	ld a, [hl]
.jmp_6F8
	cp a, $03
	jr z, .findCheckpoint
	dec a
.findCheckpoint
	ld bc, $030C
	cp a, $07
	jr c, .gotoCheckpoint
	ld bc, $0734
	cp a, $0B
	jr c, .gotoCheckpoint
	ld bc, $0B5C
	cp a, $0F
	jr c, .gotoCheckpoint
	ld bc, $0F84
	cp a, $13
	jr c, .gotoCheckpoint
	ld bc, $13AC
	cp a, $17
	jr c, .gotoCheckpoint
	ld bc, $17D4
.gotoCheckpoint
	ld [hl], b
	inc l
	ld [hl], 0
	ld a, c
	ld [$C0AB], a		; "progress" in level, used to spawn enemies?
	call DrawInitialScreen		; draw first screen of the level
	ld hl, vBGMap0 + SCRN_VX_B + 11 ; $982B		; right next to the coins
	ld [hl], " "
	inc l
	ldh a, [hWorldAndLevel]
	ld b, a
	and a, $F0
	swap a
	ldi [hl], a
	ld a, b
	and a, $0F
	inc l
	ld [hl], a
	ld hl, vBGMap1
	ld de, PauseText
	ld b, PauseTextEnd - PauseText
.loop
	ld a, [de]
	ldi [hl], a
	inc de
	dec b
	jr nz, .loop
	xor a
	ldh [hGameState], a
	ld [wInvincibilityTimer], a
	ld a, $C3
	ldh [rLCDC], a
	call StartLevelMusic
	xor a
	ldh [rIF], a
	ldh [hScrollX], a
	ld [$C0D2], a			; starts incrementing at end of level
	ldh [$FFEE], a			; source of collision?
	ld [wGameTimerExpiringFlag], a
	ldh [rTMA], a
	ld hl, wGameTimer + 1	; ones and tens
	ldi [hl], a				; hundreds
	ld [hl], $04
	ld a, $28
	ld [wGameTimer], a
	ld a, $5B
	ldh [$FFE9], a			; some sort of index in the level
	ldh a, [hLevelIndex]
	ld c, $0A				; submarine
	cp a, $05
	jr z, .autoscroll
	ld c, $0C				; airplane
	cp a, $0B
	jr nz, .out
.autoscroll
	ld a, $0D
	ldh [hGameState], a		; autoscroll
	ld a, [wAnimIndex]			; animation index
	and a, $F0
	or c
	ld [wAnimIndex], a
.out
	call InitEnemySlots
	ei
	ret

PauseText::
	db " ♥pause♥ "
PauseTextEnd::

StartLevelMusic::
	ld a, [wInvincibilityTimer]
	and a
	ret nz
	ld a, BANK(InitSound)
	ld [MBC1RomBank], a	; no need to save rom bank, interrupts are disabled
	call InitSound
	ldh a, [hActiveRomBank]
	ld [MBC1RomBank], a
	ldh a, [$FFF4]		; underground?
	and a
	jr nz, .underground
	ldh a, [hLevelIndex]
	ld hl, .musicByLevel
	ld e, a
	ld d, $00
	add hl, de
	ld a, [hl]
	ld [wActiveMusic], a
	ret

.underground
	ld a, 4				; underground music
	ld [wActiveMusic], a
	ret

.musicByLevel
	db 7, 7, 3
	db 8, 8, 5
	db 7, 3, 3
	db 6, 6, 5

pauseOrReset:: ; 7DA
	ldh a, [hJoyHeld]
	and a, A_BUTTON | B_BUTTON | START | SELECT
	cp a, A_BUTTON | B_BUTTON | START | SELECT
	jr nz, .noReset
	jp Init				; if at any point A+B+Start+Select are pressed, reset

.noReset
	ldh a, [hJoyPressed]
	bit START_BIT, a			; start button bit. (Un)Pause the game!
	ret z
	ldh a, [hGameState]
	cp a, STATE_LOAD_MENU
	ret nc				; <= gamestates mostly relating to normal gameplay
	ld hl, rLCDC
	ldh a, [hGamePaused]
	xor a, 1			; (un)pause game
	ldh [hGamePaused], a
	jr z, .unpaused
	set 5, [hl]			; Display window with pause
	ld a, 1				; Pause music
.pauseMusic
	ldh [hPauseUnpauseMusic], a
	ret

.unpaused
	res 5, [hl]			; disable windows again
	ld a, 2				; Unpause music
	jr .pauseMusic

; this draw the first screen of the level. The rest is dynamically loaded
DrawInitialScreen::
	ld hl, InitialStateData
	ld de, wLevelData	; level data: mario data etc.
	ld b, $51			; Bug? One byte too much
.copyLoop				; some sort of initialisation
	ldi a, [hl]
	ld [de], a
	inc de
	dec b
	jr nz, .copyLoop
	ldh a, [hSuperStatus]
	and a
	jr z, .drawLevel	; jump if small mario
	ld a, $10
	ld [wAnimIndex], a		; animation index. upper nibble is 1 of large mario
.drawLevel					; does weird things in autoscroll
	ld hl, hColumnIndex
	xor a
	ld b, 6
.clearLoop
	ldi [hl], a
	dec b
	jr nz, .clearLoop	; clears FFE6-FFEB
	ldh [$FFA3], a		; switches between 0 and 8, depending on scroll coord
	ld [$C0AA], a		; and yet another scrolling thing
	ld a, $40
	ldh [$FFE9], a		; level index of some sort
	ld b, SCRN_X_B			; an underground level is only 20 tiles wide, no scroll
	ldh a, [hGameState]
	cp a, $0A			; pipe going underground
	jr z, .drawLoop
	ldh a, [hLevelIndex]
	cp a, $0C			; start menu doesn't scroll
	jr z, .drawLoop
	ld b, SCRN_X_B + 7	; load 27 tiles (20 visible, 7 preloaded)
.drawLoop
	push bc
	call LoadNextColumn	; load a column of the level into C0B0
	call DrawColumn		; draw it onscreen
	pop bc
	dec b
	jr nz, .drawLoop
	ret

; called from main gameplay subroutine
; player "entity" (enemy, powerup) collision
EntityCollision:: ; 84E
	ldh a, [hStompChainTimer]
	and a
	jr z, .skip			; don't decrement below zero
	dec a
	ldh [hStompChainTimer], a
.skip
	ld de, -16
	ld b, $0A
	ld hl, $D190
.enemyLoop
	ld a, [hl]
	cp a, $FF
	jr nz, .jmp_868
.nextEnemy
	add hl, de
	dec b
	jr nz, .enemyLoop	; loop over all enemies/entities
	ret

.jmp_868
	ldh [$FFFB], a
	ld a, l
	ldh [$FFFC], a
	push bc
	push hl
	ld bc, $000A
	add hl, bc
	ld c, [hl]			; D1xA mortal + width + height
	inc l
	inc l
	ld a, [hl]			; D1xC health?
	ldh [$FF9B], a
	ld a, [wMarioPosY]		; Mario Y pos
	ld b, a
	ldh a, [hSuperStatus]
	cp a, 2
	jr nz, .jmp_88E
	ld a, [wAnimIndex]
	cp a, $18
	jr z, .jmp_88E
	ld a, -2
	add b
	ld b, a
.jmp_88E
	ld a, b
	ldh [hBoundingBoxTop], a		; bounding box top?
	ld a, [wMarioPosY]     ; Mario Y pos
	add a, 6
	ldh [hBoundingBoxBottom], a		; bounding box bottom?
	ld a, [wMarioPosX]		; Mario X pos
	ld b, a
	sub a, 3
	ldh [hBoundingBoxLeft], a		; bounding box left?
	ld a, 2
	add b
	ldh [hBoundingBoxRight], a		; BB right
	pop hl
	push hl
	call BoundingBoxCollision		; hitbox detection
	and a
	jp z, .noCollision
	ldh a, [$FFFC]
	cp a, $90			; powerups only appear in the last slot
	jp z, .powerUpCollision
	ldh a, [$FFFB]		; gets overwritten immediately. Bug?
	ldh a, [hGameState]
	cp a, $0D			; autoscroll
	jr z, .jmp_8C3
	ld a, [wInvincibilityTimer]
	and a
	jr z, .jmp_8C7
.jmp_8C3
	dec l
	jp .jmp_94B

.jmp_8C7
	ld a, [wMarioPosX]		; mario x pos
	add a, 6
	ld c, [hl]
	dec l
	sub c
	jr c, .jmp_94B
	ld a, [wMarioPosX]
	sub a, 6
	sub b
	jr nc, .jmp_94B
	ld b, [hl]
	dec b
	dec b
	dec b
	ld a, [wMarioPosY]
	sub b
	jr nc, .jmp_94B
	dec l
	dec l
	push hl
	ld bc, $000A
	add hl, bc
	bit 7, [hl]			; 7 bit set, enemy can't die
	pop hl
	jr nz, .out
	call Call_A10		; hit enemy
	call StombEnemy
	and a
	jr z, .out
	ld hl, wMarioOnGround	; 1 if on ground
	ld [hl], 0
	dec l
	dec l
	ld [hl], $D			; C208
	dec l
	ld [hl], 1			; wJumpStatus
	ld hl, wAnimIndex		; animation
	ld a, [hl]
	and a, $F0
	or a, $04			; flying
	ld [hl], a
.enemyKilled
	ld a, SOUND_STOMP
	ld [wSquareSFX], a		; stomp sound
	ld a, [wMarioPosX]		; X pos
	add a, -4
	ldh [hFloatyX], a	; todo comment
	ld a, [wMarioPosY]
	sub a, $10
	ldh [hFloatyY], a
	ldh a, [$FF9E]
	ldh [hFloatyControl], a
	ldh a, [hStompChainTimer]
	and a
	jr z, .resetStompChain
	ldh a, [hStompChain]
	cp a, 3				; maximum chain of 3
	jr z, .calculateScoreReward
	inc a
	ldh [hStompChain], a
.calculateScoreReward
	ld b, a
	ldh a, [hFloatyControl]
	cp a, $50			; Floaties above $50 are not score but coins and 1UPs
	jr z, .resetStompChain
.loop					; Shift BCD encoded score reward. Works out to a 
	sla a				; multiplication by 2, except for a weird jump
	dec b				; from 800 → 1000
	jr nz, .loop
	ldh [hFloatyControl], a
.resetStompChainTimer	; 50 frames? 5/6ths of a second? No wonder chaining
	ld a, $32			; is so hard in this game
	ldh [hStompChainTimer], a
	jr .out

.resetStompChain
	xor a
	ldh [hStompChain], a
	jr .resetStompChainTimer

.jmp_94B	; enemy side hit?
	dec l
	dec l
	ld a, [wInvincibilityTimer]
	and a
	jr nz, .jmp_974			; try to kill enemy?
	ldh a, [hSuperStatus]
	cp a, 3
	jr nc, .out				; if superstatus is 4 (or more?), Mario has some
	call Call_2A44			; i-frames
	and a
	jr z, .out
	ldh a, [hSuperStatus]
	and a       ; mario small
	jr nz, .injureAndOut
	call KillMario
.out
	pop hl
	pop bc
	ret

.noCollision
	pop hl
	pop bc
	jp .nextEnemy

.injureAndOut
	call InjureMario
	jr .out

.jmp_974
	call Call_2B06			; like 2AXX calls, a lookup into tables
	and a					; between $3000 and $4000
	jr z, .out
	jr .enemyKilled

.powerUpCollision
	ldh a, [$FFFB]
	cp a, POWERUP_MUSHROOM			; mushroom
	jr z, .pickupMushroom
	cp a, POWERUP_STAR				; Star
	jr z, .pickupStar
	cp a, POWERUP_ONE_UP			; 1 UP
	jr z, .pickup1UP
	cp a, POWERUP_FLOWER			; Flower
	jr nz, .out
.pickupFlower
	ldh a, [hSuperStatus]
	cp a, 2					; if we lost our Super before picking up
	jr nz, .becomeSuper		; the flower, it functions like a mushroom
	ldh [hSuperballMario], a
.playPowerUpSound
	ld a, 4
	ld [wSquareSFX], a
.spawn1000ScoreFloaty
	ld a, $10
	ldh [hFloatyControl], a
.positionFloaty
	ld a, [wMarioPosX]
	add a, -4
	ldh [hFloatyX], a		; todo comment
	ld a, [wMarioPosY]
	sub a, 16
	ldh [hFloatyY], a			; Y position of floaty number
	dec l
	dec l
	dec l
	ld [hl], $FF
	jr .out

.pickupMushroom
	ldh a, [hSuperStatus]
	cp a, 2
	jr z, .spawn1000ScoreFloaty
.becomeSuper
	ld a, 1
	ldh [hSuperStatus], a
	ld a, $50
	ldh [hTimer], a
	jr .playPowerUpSound

.pickupStar
	ld a, $F8
	ld [wInvincibilityTimer], a
	ld a, $0C
	ld [wActiveMusic], a				; Galop Infernal
	jr .spawn1000ScoreFloaty

.pickup1UP
	ld a, $FF
	ldh [hFloatyControl], a		; 1UP floaty
	ld a, SOUND_ONE_UP
	ld [wSquareSFX], a				; life up
	ld a, 1
	ld [wLivesEarnedLost], a
	jr .positionFloaty

InjureMario:: ; 9E0
	ld a, 3
	ldh [hSuperStatus], a
	xor a
	ldh [hSuperballMario], a
	ld a, $50
	ldh [hTimer], a
	ld a, 6
	ld [wSquareSFX], a			; injury music
	ret

KillMario:: ; 9F1
	ld a, [$D007]
	and a
	ret nz
	ld a, STATE_PREPARE_DEATH	; pre dying
	ldh [hGameState], a
	xor a
	ldh [hSuperballMario], a; superball capability
	ldh [rTMA], a
	ld a, 2
	ld [wActiveMusic], a			; sound effect
	ld a, SPRITE_HIDDEN             ; make mario invisible
	ld [wMarioVisible], a
	ld a, [wMarioPosY]			; Mario Y pos
	ld [wMarioDeathY], a			; death Y pos?
	ret

; called when a hit is detected on an enemy?
Call_A10::
	push hl
	push de
	ldh a, [$FF9B]			; enemy... health?
	and a, %11000000
	swap a
	srl a
	srl a				; put two upper bits in lowest position
	ld e, a
	ld d, $00
	ld hl, .data_A29
	add hl, de
	ld a, [hl]
	ldh [$FF9E], a
	pop de
	pop hl
	ret

.data_A29
; corresponding top nibbles
;      0-3  4-7  8-B  C-F
	db $01, $04, $08, $50

; called when Mario hits a bouncing block, to hit the enemy above it
Call_A2D:: ; A2D
	ldh a, [$FFEE]
	and a
	ret z
	cp a, $C0
	ret z				; return if no collision, and if not with a coin
	ld de, $0010
	ld b, $0A
	ld hl, $D100		; enemies. and hittable objects
.loop
	ld a, [hl]
	cp a, $FF			; ff means no object
	jr nz, .checkEnemyHit
.nextEnemy
	add hl, de
	dec b
	jr nz, .loop
	ret

.checkEnemyHit
	push bc
	push hl
	ld bc, $000A
	add hl, bc			; D1xA, lower 7 bits is width + height, 7th bit is ?
	bit 7, [hl]
	jr nz, .noHit		; bit 7 is immortality?
	ld c, [hl]
	inc l
	inc l				; D1xC, health?
	ld a, [hl]
	ldh [$FF9B], a		; used in Call_A10 at least
	pop hl
	push hl
	inc l
	inc l
	ld b, [hl]			; D1x2 Y pos
	ld a, [wMarioPosY]		; player Y pos
	sub b				; Y coordinates are inverted
	jr c, .noHit		; enemy needs to be above player
	ld b, a
	ld a, $14
	sub b
	jr c, .noHit		; but not too much above the player either
	cp a, 7			; A contains $14 - (playerY - enemyY) and has to be < 7
	jr nc, .noHit		; meaning playerY - enemyY has to be at least $D
	inc l
	ld a, c				; c contains the width + height
	and a, $70			; just width
	swap a
	ld c, a
	ld a, [hl]			; D1x3 X pos, points to left bound of leftmost tile
.loopR
	add a, 8			; one tile per width
	dec c
	jr nz, .loopR
	ld c, a				; C contains right bound of enemy
	ld b, [hl]			; B contains left bound of enemy
	ld a, [wMarioPosX]		; Mario X pos
	sub a, 6			; Mario is 12 pixels wide
	sub c
	jr nc, .noHit		; left bound has to be smaller than right bound of enemy
	ld a, [wMarioPosX]
	add a, 6
	sub b
	jr c, .noHit
	dec l
	dec l
	dec l				; D1x0
	push de
	call Call_A10
	call Call_2A23		; prepares death animation
	pop de
	and a
	jr z, .noHit
	ld a, [wMarioPosX]		; X pos
	add a, -4				; or -4
	ldh [hFloatyX], a
	ld a, [wMarioPosY]		; Y pos
	sub a, $10
	ldh [hFloatyY], a
	ldh a, [$FF9E]
	ldh [hFloatyControl], a
.noHit
	pop hl
	pop bc
	jp .nextEnemy

; TODO clean up these comments
; collision detection between enemy and bounding box defined by
; T B L R FFA0 FFA1 FFA2 FF8F (-_-)
; HL contains D1x0 of enemy under consideration?
; C is some sort is width? XY in both nibbles?
BoundingBoxCollision:: ; AAF
	inc l
	inc l				; D1x2 Y pos
	ld a, [hl]
	add a, 8			; Y pos is top left of bottom left object tile
	ld b, a				; so add 8 to get coordinate of bottom of enemy
	ldh a, [hBoundingBoxTop]		; top of bounding box?
	sub b				; top - bottom
	jr nc, .noCollision ; NC if bottom < top (don't forget Y coords grow downwards)
	ld a, c
	and a, $0F			; lower nibble, height in tiles?
	ld b, a
	ld a, [hl]			; still Y pos
.loopHeight
	dec b
	jr z, .checkBottomOfBB
	sub a, 8			; subtract (c & 0F) tiles
	jr .loopHeight

.checkBottomOfBB
	ld b, a				; B contains top of enemy
	ldh a, [hBoundingBoxBottom]		; bottom Y
	sub b
	jr c, .noCollision	; C if top > bottom (Y coords grow downwards)
; X detection
	inc l				; D1x3 X pos
	ldh a, [hBoundingBoxRight]		; right BB x
	ld b, [hl]			; left X of enemy
	sub b
	jr c, .noCollision	; C if left X of enemy > right BB X
	ld a, c
	and a, $70			; upper nibble, but only 3 bits
	swap a
	ld b, a
	ld a, [hl]			; still X pos
.loopWidth
	add a, 8			; add width tiles to get the right bound of the enemy
	dec b
	jr nz, .loopWidth
	ld b, a
	ldh a, [hBoundingBoxLeft]		; left BB x
	sub b
	jr nc, .noCollision	; NC if left BB x > right X of enemy
	ld a, 1				; collision detected
	ret

.noCollision
	xor a
	ret

; has to do with Mario riding on platforms and blocks
Call_AEA:: ; AEA
	ld a, [wJumpStatus]		; jump status
	cp a, MARIO_ASCENDING
	ret z
	ld de, $0010
	ld b, $0A
	ld hl, $D100		; enemies again
.loop
	ld a, [hl]
	cp a, $FF
	jr nz, .checkEnemy
.nextEnemy
	add hl, de
	dec b
	jr nz, .loop
	ret

.checkEnemy
	push bc
	push hl
	ld bc, $000A
	add hl, bc
	bit 7, [hl]			; mortality?
	jp z, .notOnTop		; only dealing with immortal enemies (platforms, blocks?)
	ld a, [hl]			; mortal bit + width + height
	and a, $0F			; just height
	ldh [hBoundingBoxTop], a		; hitbox? temporary storage?
	ld bc, -8
	add hl, bc			; D1x2 Y pos
	ldh a, [hBoundingBoxTop]		; ...why? do we jump into this?
	ld b, a
	ld a, [hl]			; Y pos
.loopT
	dec b
	jr z, .break		; decrement before subtracting tile height, as the Y pos
	sub a, $08			; already corresponds to the top bound
	jr .loopT

.break
	ld c, a				; enemy top bound
	ldh [hBoundingBoxTop], a
	ld a, [wMarioPosY]		; player y pos
	add a, 6			; todo is mario 6 units tall or so?
	ld b, a				; mario bottom bound
	ld a, c
	sub b
	cp a, 7				; mario has to be less than 8 pixels above the enemy
	jr nc, .notOnTop
	inc l
	ld a, [wMarioPosX]		; mario x pos
	ld b, a
	ld a, [hl]			; enemy x pos
	sub b
	jr c, .checkRightBound	; if enemy x < mario x, ok
	cp a, 3
	jr nc, .notOnTop		; maximum 2 units of overhang? why not add before..
.checkRightBound
	push hl
	inc l
	inc l
	inc l
	inc l
	inc l
	inc l
	inc l
	ld a, [hl]			; D1xA
	and a, $70			; width
	swap a
	ld b, a
	pop hl
	ld a, [hl]			; x pos
.loopR
	add a, 8
	dec b
	jr nz, .loopR		; find right bound
	ld b, a
	ld a, [wMarioPosX]		; mario x pos
	sub b				; 
	jr c, .jmp_B5D
	cp a, 3
	jr nc, .notOnTop
.jmp_B5D
	dec l
	ldh a, [hBoundingBoxTop]		; enemy top Y bound
	sub a, $0A
	ld [wMarioPosY], a		; position Mario 10 units above
	push hl
	dec l
	dec l				; D1x0
	call StombEnemy
	pop hl
	ld bc, $0009
	add hl, bc
	ld [hl], 1		; D1xB
	xor a
	ld hl, wJumpStatus
	ldi [hl], a			; C207 jump status
	ldi [hl], a			; C208
	ldi [hl], a			; C209
	ld [hl], 1			; C20A 1 if mario on the ground
	ld hl, wMarioMomentum		; two INC L's would've been cheaper >_<
	ld a, [hl]
	cp a, $07			; momentum?
	jr c, .out
	ld [hl], $06
.out
	pop hl
	pop bc
	ret

.notOnTop
	pop hl
	pop bc
	jp .nextEnemy

; prepare Mario's dying sprite. And some variables
PrepareDeath:: ; B8D
	ld hl, wMarioOAMBuffer	; Mario's 4 objects todo
	ld a, [wMarioDeathY]	; death Y position?
	ld c, a
	sub a, 8
	ld d, a
	ld [hl], a				; Y position
	inc l
	ld a, [wMarioPosX]		; Mario's screen x position
	add a, -8				; subtract 8
	ld b, a
	ldi [hl], a				; X position
	ld [hl], $0F			; todo mario dying object top
	inc l
	ld [hl], SPRITE_NO_FLIP	; OAM bits
	inc l
	ld [hl], c				; Y position
	inc l
	ld [hl], b				; X position
	inc l
	ld [hl], $1F			; bottom dying object
	inc l
	ld [hl], SPRITE_NO_FLIP	; OAM, flip
	inc l
	ld [hl], d				; Y position
	inc l
	ld a, b
	add a, 8
	ld b, a
	ldi [hl], a				; X position
	ld [hl], $0F			; top right dying mario object (mirrored)
	inc l
	ld [hl], SPRITE_XFLIP 	; OAM X flip
	inc l
	ld [hl], c				; Y position
	inc l
	ld [hl], b				; X position
	inc l
	ld [hl], $1F			; bottom dying object
	inc l
	ld [hl], SPRITE_XFLIP 	; OAM X flip
	ld a, STATE_DYING		; dying animation
	ldh [hGameState], a
	xor a
	ld [$C0AC], a
	ldh [hSuperStatus], a
	ldh [$FFF4], a
	call ClearSprites
	ret

; Dying animation
HandleDying:: ; BD6
	ld a, [$C0AC]		; death animation counter
	ld e, a				; use DE as an offset in the table
	inc a
	ld [$C0AC], a
	ld d, 0
	ld hl, Data_C19
	add hl, de
	ld b, [hl]			; b = Y speed
	ld a, b
	cp a, $7F			; sentinel
	jr nz, .next
	ld a, [$C0AC]
	dec a
	ld [$C0AC], a
	ld b, 2				; coast with the same speed of 2
.next
	ld hl, wMarioOAMBuffer   		; 3rd object, first mario object
	ld de, 4						; 4 bytes per object
	ld c, 4							; 4 object in total
.loop
	ld a, b
	add [hl]
	ld [hl], a						; first byte is Y position
	add hl, de						; next object
	dec c
	jr nz, .loop
	cp a, $B4						; end animation when he goes low enough
	ret c
	ld a, [wGameTimerExpiringFlag]
	cp a, $FF
	jr nz, .deathState
	ld a, $3B						; Prepare time up
	jr .out

.deathState
	ld a, $90
	ldh [hTimer], a					; 90 frames, 1.5 seconds
	ld a, STATE_DEAD
.out
	ldh [hGameState], a
	ret

Data_C19::
	db -2, -2, -2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, 0, 0, -1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, $7F

; end of level gate, music
HandleLevelEndFX:: ; C40
	ld hl, hTimer
	ld a, [hl]
	and a
	jr z, .jmp_C4B
	call Call_1736
	ret
.jmp_C4B
	ld a, [$D007]
	and a
	jr nz, .jmp_C55
	ld a, $40
	ldh [hTimer], a
.jmp_C55
	ld a, STATE_SCORE_COUNTING_DOWN		; score counting down
	ldh [hGameState], a
	xor a
	ld [wGameTimerExpiringFlag], a
	ldh [rTMA], a
	ldh a, [hWorldAndLevel]
	and a, $0F				; select just the level
	cp a, 3					; boss level
	ret nz
	call ExplodeAllEnemies
	ldh a, [hWorldAndLevel]
	cp a, $43				; last level
	ret nz
	ld a, STATE_LEVEL_END		; don't count down score after Tatanga
	ldh [hGameState], a
	ret

; explode enemies, count down timer and add to score
HandleLevelEndCountdownTimer:: ; C73
	ldh a, [hWorldAndLevel]
	and a, $0F
	cp a, 3
	jr nz, .countdown
	xor a
	ld [$C0AB], a
	call Call_2491		; explodes the enemies?
.countdown
	ldh a, [hTimer]
	and a
	ret nz
	ld hl, wGameTimer + 1
	ldi a, [hl]			; ones and tens
	ld b, [hl]			; hundreds
	or b
	jr z, .endLevel
	ld a, 1
	ld [wGameTimer], a
	homecall UpdateTimerAndFloaties		; why
	ld de, $0010
	call AddScore
	ld a, 1
	ldh [hTimer], a
	xor a
	ld [wGameTimerExpiringFlag], a
	ld a, [wGameTimer + 1]
	and a, 1
	ret nz
	ld a, $0A
	ld [wSquareSFX], a
	ret

.endLevel
	ld a, STATE_LEVEL_END
	ldh [hGameState], a
	ld a, $26
	ldh [hTimer], a
	ret

; Winning
HandleLevelEnd:: ; CCB
	ldh a, [hTimer]
	and a
	ret nz
	xor a
	ld [wGameTimerExpiringFlag], a
	ldh [rTMA], a
	ldh a, [hWorldAndLevel]
	and a, $0F
	cp a, 3			; boss level
	ld a, $1C		; smth to do with the gate to Daisy
	jr z, .bossLevel
	ld a, [wMarioPosY]	; mario on screen Y position
	cp a, $60
	jr c, .bonusGame
	cp a, $A0
	jr nc, .bonusGame
	ld a, $08		; increment level
	jr .changeState

.bonusGame
	ld a, 2			; bonus game is stored in bank 2
	ldh [hActiveRomBank], a
	ld [MBC1RomBank], a
	ld a, STATE_GOTO_BONUS_GAME		; bonus game state
.changeState
	ldh [hGameState], a
	ret

.bossLevel
	ldh [hGameState], a
	ld a, 3
	ld [MBC1RomBank], a
	ldh [hActiveRomBank], a
	ld hl, hLevelIndex
	ld a, [hl]
	ldh [$FFFB], a
	ld [hl], $0C		; The end of game hangar is stored in level 13 of bank 3
	inc l				; hl ← level screen
	xor a
	ldi [hl], a			; hl = hScreenIndex
	ldi [hl], a			; hl = hColumnIndex
	ldh [$FFA3], a
	inc l
	inc l
	ld a, [hl]			; ffe9, first not yet loaded column
	ldh [$FFE0], a
	ld a, $06			; next state lasts 6 frames
	ldh [hTimer], a
	ldh a, [hWorldAndLevel]
	and a, $F0
	cp a, $40
	ret nz
	xor a
	ldh [$FFFB], a
	ld a, $01
	ld [$C0DE], a
	ld a, $BF
	ldh [$FFFC], a
	ld a, $FF
	ldh [hTimer], a
	ld a, $27			; tatanga dying
	ldh [hGameState], a
	call InitSound
	ret

HandlePrepareNextLevel:: ; D49
.world1Tiles
	di
	ld a, c
	ld [MBC1RomBank], a
	ldh [hActiveRomBank], a	; todo
	xor a
	ldh [rLCDC], a		; turn off LCD
	call PrepareTiles		; again? why????
	jp .out

.entryPoint:: ; D49
	ld hl, hTimer
	ld a, [hl]
	and a
	ret nz
	ld a, [$DFF9]
	and a
	ret nz
	ldh a, [hLevelIndex]
	inc a
	cp a, $0C			; 4*3 levels + credits?
	jr nz, .incrementLevel
	xor a
.incrementLevel
	ldh [hLevelIndex], a
	ldh a, [hWorldAndLevel]
	inc a
	ld b, a
	and a, $0F
	cp a, 4
	ld a, b
	jr nz, .notNextWorld
	add a, $10 - 3			; Add one to the World after 3 Levels
.notNextWorld
	ldh [hWorldAndLevel], a
.loadWorldTiles
	and a, $F0			; upper nibble is the world
	swap a
	cp a, 1
	ld c, 2				; world 1 tiles are in rom bank 2
.jmp_D75
	jr z, .world1Tiles	; if world 1, redo work, and in a different subroutine?
	cp a, 2
	ld c, 1				; world 2 tiles are in rom bank 1
	jr z, .jmp_D85
	cp a, 3				; world 3 tiles are in rom bank 3
	ld c, 3
	jr z, .jmp_D85
	ld c, 1				; world 4 tiles are in rom bank 1
.jmp_D85
	ld b, a
	di
	ld a, c
	ld [MBC1RomBank], a
	ldh [hActiveRomBank], a ; todo
	xor a
	ldh [rLCDC], a
	ld a, b
	dec a
	dec a
	sla a				; world 2 → 0, world 3 → 2, world 4 → 4. Index in table
	ld d, 0				; Probably because world 2 and world 4 tiles
	ld e, a				; are in the same bank
	ld hl, .enemyTileOffset
	push de
	add hl, de
	ld e, [hl]
	inc hl
	ld d, [hl]
	ld hl, vChars1 + $200 ; $8A00
.enemyTileLoop
	ld a, [de]
	ldi [hl], a
	inc de
	push hl
	ld bc, $10000 - $8DD0
	add hl, bc
	pop hl
	jr nc, .enemyTileLoop	; fill from 8A00 to 8DD0. World specific enemies
	pop de
	ld hl, .backdropTileOffset
	add hl, de
	ld e, [hl]
	inc hl
	ld d, [hl]
	push de
	ld hl, vChars2 + $310 ; $9310
.backdropTileLoop
	ld a, [de]
	ldi [hl], a
	inc de
	ld a, h
	cp a, $97
	jr nz, .backdropTileLoop	; fill from 9310 to 9700. Backdrop
	pop hl
	ld de, $02C1			; Tile 2C is the animated tile
	add hl, de
	ld de, $C600			; animated tile backup
	ld b, 8
.animatedTileLoop
	ldi a, [hl]
	ld [de], a
	inc hl				; Skip every first byte of a pair, which is always 00
	inc de				; It's 1BPP encoded, with padding
	dec b
	jr nz, .animatedTileLoop
.out
	xor a
	ldh [rIF], a
	ld a, %11000011 ; $C3
	ldh [rLCDC], a
	ei
	ld a, 3
	ldh [hScreenIndex], a	; do all levels start on screen 3 todo
	xor a
	ld [$C0D2], a
	ldh [hIsUnderground], a
	ld a, STATE_RESET_TO_CHECKPOINT
	ldh [hGameState], a
	call SetIsBackgroundAnimated
	ret

; todo
.enemyTileOffset
	dw $4032, $4032, $47F2

.backdropTileOffset
	dw $4402, $4402, $4BC2

; leaving bonus game?
HandleLeaveBonusGame:: ; DF9
	di
	xor a
	ldh [rLCDC], a	; turn off lcd
	call PrepareHUD
	call DisplayCoins
	call UpdateLives.displayLives
	xor a
	ldh [rIF], a
	ld a, %11000011 ; $C3
	ldh [rLCDC], a
	ei
	ld a, STATE_8
	ldh [hGameState], a
	ldh [$FFB1], a
	ret

GameState_1C:: ; E15
	ldh a, [hTimer]		; wait 6 frames
	and a
	jr z, .nextState
	call LoadNextColumn	; doesn't seem to be necessary
	xor a
	ld [$C0AB], a
	call Call_2491			; explode enemies?
	call Call_1736		; mario animation?
	ret

.nextState
	ld a, $40			; 2/3 second
	ldh [hTimer], a
	ld hl, hGameState
	inc [hl]
	ret					; 1C → 1D

GameState_1D:: ; E31
	xor a
	ld [$C0AB], a
	call Call_2491
	ldh a, [hTimer]
	and a
	ret nz
	ldh a, [$FFE0]		; first column past the end
	sub a, 2			; above gate
	cp a, $40
	jr nc, .nowrap		; in case of wrap around
	add a, SCRN_VX_B	; one screen width
.nowrap
	ld l, a
	ld h, HIGH(vBGMap0) ; $98
	ld de, 9 * SCRN_VX_B ; bottom of the gate is 9 tiles under top
	add hl, de
	ld a, l
	ldh [$FFE0], a		; contains low byte of location of bottom of gate
	ld a, 5
	ldh [$FFFC], a		; gate consists of 4 segments (+1 because of shitty logic)
	ld a, 8
	ldh [hTimer], a
	ld hl, hGameState
	inc [hl]
	ret

; Open gate
HandleGateOpening:: ; E5D
	ldh a, [hTimer]
	and a
	ret nz
	ldh a, [$FFFC]
	dec a
	jr z, .gateIsOpen
	ldh [$FFFC], a
	ldh a, [$FFE0]
	ld l, a
	ld h, $99
	sub a, $20
	ldh [$FFE0], a
.waitHBlank
	ldh a, [$FF41]
	and a, %11
	jr nz, .waitHBlank
	ld [hl], " "
	ld a, $08
	ldh [hTimer], a
	ld a, $0B
	ld [wSquareSFX], a		; sound effect
	ret

.gateIsOpen
	ld a, $10
	ldh [hTimer], a
	ld a, BANK(InitSound)
	ldh [hActiveRomBank], a
	ld [MBC1RomBank], a
	call InitSound
	ld hl, hGameState
	inc [hl]			; 1E → 1F
	ret

; gate is open
HandleGateOpen:: ; E96
	ldh a, [hTimer]
	and a
	ret nz
	xor a
	ld [$C0D2], a		; increments at end of level
	ld [wJumpStatus], a		; jump status
	inc a
	ldh [hIsUnderground], a		; tiens, non zero in underground
	ld hl, hGameState
	inc [hl]			; 1F → 20
	ret

; Mario walks/flies off screen
HandleTatangaDead::
MarioStartsWalkingOffscreen:: ; EA9
	call .walkRight
	ld a, [wMarioPosX]		; mario on screen X
	cp a, $C0
	ret c
	ld a, $20
	ldh [hTimer], a
	ld hl, hGameState
	inc [hl]			; 20 → 21
	ret

.walkRight
	ld a, $10
	ldh [hJoyHeld], a	; simulate pressing Right button todo
	ld a, [wAnimIndex]		; animation index
	and a, $0F
	cp a, $0A			; animations >= $0A are sub or airplane
	call c, Call_17BC
	call Call_16F5		; animate and move mario
	ret

; preparing Fake Daisy
HandleMarioWalksOffscreen:: ; ECD
	ldh a, [hTimer]
	and a
	ret nz
	call .prepareMarioAndDaisy
	xor a
	ldh [$FFEA], a		; render status?
	ldh [$FFA3], a		; switches between 0 and 8..
	ld a, $A1
	ldh [hTimer], a
	ld a, $0F
	ld [wActiveMusic], a		; Daisy music
	ld hl, hGameState
	inc [hl]			; 21 → 22
	ret

.prepareMarioAndDaisy::
	ld hl, wMarioPosY	; mario y position
	ld [hl], 126
	inc l
	ld [hl], 176		; mario x
	inc l
	ld a, [hl]			; anim index
	and a, $F0
	ld [hl], a
	ld hl, $C210
	ld de, InitialStateData
	ld b, 16
.loop
	ld a, [de]
	ldi [hl], a
	inc de
	dec b
	jr nz, .loop

	ld hl, wEntityPosY
	ld [hl], 126		; object Y pos?
	inc l
	ld [hl], 0			; object X pos?
	inc l
	ld [hl], $22		; Daisy :)
	inc l
	inc l
	ld [hl], SPRITE_XFLIP	; flipped
	ret


; scroll the screen
HandleScrollToDaisy:: ; F12
	ldh a, [hTimer]
	and a
	jr z, .nextState
	ld hl, hScrollX
	inc [hl]
	call LoadColumns		; loads in columns?
	ld hl, wMarioPosX		; mario x pos
	dec [hl]
	ld hl, wEntityPosX		; fake daisy x pos
	dec [hl]
.animateMarioAndReturn
	call Call_1736
	ret

.nextState
	ldh a, [$FFFB]		; temporarily stores level index?
	ldh [hLevelIndex], a
	ld hl, hGameState
	inc [hl]			; 22 → 23
	ret

HandleWalkToFakeDaisy:: ; F33
	ld a, D_RIGHT		; right button
	ldh [hJoyHeld], a
	call Call_17BC
	call Call_16F5
	ld a, [wMarioPosX]
	cp a, 76			; almost middle of screen
	ret c
	ld a, [wAnimIndex]
	and a, $F0
	ld [wAnimIndex], a		; mario standing still
	ld a, [$FFE0]			; top of gate?
	sub a, $40				; two tiles up
	add a, 4				; four to the right
	ld b, a
	and a, $F0
	cp a, $C0
	ld a, b
	jr nz, .nowrap
	sub a, $20
.nowrap
	ldh [hTextCursorLo], a
	ld a, HIGH(vBGMap0) ; $98
	ldh [hTextCursorHi], a
	xor a
	ldh [$FFFB], a
	ld hl, hGameState
	inc [hl]
	jr HandleScrollToDaisy.animateMarioAndReturn

; Fake Daisy speaking
HandleFakeDaisySpeaking:: ; F6A
	ld hl, ThankYouText
	call PrintVictoryMessage
	cp a, "@"		; end of speech
	ret nz
	ld hl, hGameState
	inc [hl]		; 24 → 25
	ld a, SPRITE_HIDDEN
	ld [wEntityVisible], a	; make sprite invisible
	ld a, 8
	ldh [hTimer], a
	ld a, 8
	ldh [$FFFB], a	; timer for morph
	ld a, $12
	ld [wActiveMusic], a	; music
	ret

PrintVictoryMessage:: ; F8A
	ldh a, [hTimer]
	and a
	ret nz
	ld a, [$FFFB]	; keeps track of how many letters were already printed
	ld e, a
	ld d, 0
	add hl, de
	ld a, [hl]
	ld b, a
	cp a, "<LINE>"
	jr z, .newline
	cp a, "@"
	ret z
	ldh a, [hTextCursorHi]
	ld h, a
	ldh a, [hTextCursorLo]
	ld l, a
.printLetter
	WAIT_FOR_HBLANK
	WAIT_FOR_HBLANK
	ld [hl], b
	inc hl
	ld a, h
	ldh [hTextCursorHi], a
	ld a, l
	and $0F
	jr nz, .nowrap
	bit 4, l
	jr nz, .nowrap
	ld a, l
	sub a, $20
.nextLetter
	ldh [hTextCursorLo], a
	inc e
	ld a, e
	ldh [$FFFB], a
	ld a, $0C
	ldh [hTimer], a
	ret

.nowrap
	ld a, l
	jr .nextLetter

.newline
	inc hl
	ldi a, [hl]			; next byte determines how many tiles to skip
	ld c, a
	ld b, 0
	ld a, [hl]
	push af
	ldh a, [hTextCursorHi]
	ld h, a
	ldh a, [hTextCursorLo]
	ld l, a
	add hl, bc
	pop bc
	inc de
	inc de
	jr .printLetter

ThankYouText::
	db "thank you mario.<LINE>", $73
	db "oh! daisy@"

; Fake Daisy morphing
HandleFakeDaisyMorphing:: ; FFD
	ldh a, [hTimer]
	and a
	ret nz
	ldh a, [$FFFB]
	dec a
	jr z, .nextState
	ldh [$FFFB], a
	and a, 1
	ld hl, MorphSprite1OAM
	jr nz, .exchangeSprite
	ld hl, MorphSprite2OAM
	ld a, 3
	ld [wNoiseSFX], a
.exchangeSprite
	call .writeSprite
	ld a, 8
	ldh [hTimer], a
	ret

.nextState
	ld hl, wEntityVisible
	ld [hl], SPRITE_VISIBLE		; make sprite visible
	ld hl, hGameState
	inc [hl]			; 25 → 26
	ret

.writeSprite
	ld de, wOAMBuffer + 4 * $7	; Daisy sprite
	ld b, 4 * 4					; 4 concomitant objects
.loop
	ldi a, [hl]
	ld [de], a
	inc e
	dec b
	jr nz, .loop
	ret

MorphSprite1OAM::
	db 120, 88, $06, $00
	db 120, 96, $06, SPRITE_XFLIP
	db 128, 88, $06, SPRITE_YFLIP
	db 128, 96, $06, SPRITE_XFLIP | SPRITE_YFLIP

MorphSprite2OAM::
	db 120, 88, $07, $00
	db 120, 96, $07, SPRITE_XFLIP
	db 128, 88, $07, SPRITE_YFLIP
	db 128, 96, $07, SPRITE_XFLIP | SPRITE_YFLIP

; Fake Daisy monster jumping away
HandleFakeDaisyJumpingAway:: ; 1055
	ldh a, [hTimer]
	and a
	ret nz
	ld hl, wEntityAnimIndex		; sprite 1 animation index?
	ld [hl], $20				; jumping enemy
	ld bc, $C218
	ld hl, Data_216D			; jumping curve
	push bc
	call Call_490D				; animates smth?
	pop hl
	dec l
	ld a, [hl]
	and a
	jr nz, .advanceMonster
	ld [hl], 1
	ld hl, wEntityAnimIndex
	ld [hl], $21				; jumping enemy on the ground
	ld a, $40
	ldh [hTimer], a
.advanceMonster
	ldh a, [hFrameCounter]
	and a, %1
	jr nz, .out
	ld hl, wEntityPosX
	inc [hl]
	ld a, [hl]
	cp a, $D0
	jr nc, .monsterOutOfView
.out
	call Call_1736
	ret

.monsterOutOfView
	ld hl, hGameState
	ld [hl], STATE_GOTO_BONUS_GAME		; go to bonus game
	ld a, 2
	ldh [hActiveRomBank], a
	ld [MBC1RomBank], a
	ret

; Shaking, explosions, blocks disappearing
; the blocks are removed by a constantly rotating bitmask ANDed with the tiles
; the bitmask goes
; 10111111 → 11100111 → 11101100 → 10001101 → 10100001 → 00100100 → 1000010 → 1000000
HandleTatangaDying::	; 1099
	ldh a, [$FFA7]
	and a
	jr nz, .screenShake
	ld a, SOUND_EXPLOSION
	ld [wNoiseSFX], a		; explosion sound effect
	ld a, $20			; one explosion every 32 frames, about half a second
	ldh [$FFA7], a
.screenShake
	xor a
	ld [$C0AB], a
	call Call_2491		; sprite animation?
	ldh a, [hTimer]
	ld c, a
	and a, %11			; shake every 4 frames
	jr nz, .disintegrateBlocks
	ldh a, [$FFFB]
	xor a, 1
	ldh [$FFFB], a
	ld b, -4
	jr z, .updateScrollY
	ld b, 4
.updateScrollY
	ld a, [wScrollY]
	add b
	ld [wScrollY], a
.disintegrateBlocks
	ld a, c
	cp a, $80
	ret nc				; start disappearing block 128 frames before the end
	and a, $20 - 1		; every 32 frames
	ret nz
	ld hl, $8DD0		; all kinds of tiles, even tho only 3 are visible
	ld bc, 34 * $10		; 34 from the start, but it skips ahead somewhere
	ldh a, [$FFFC]		; starts at BF 10111111
	ld d, a
.maskTileRow
	WAIT_FOR_HBLANK
	ld a, [hl]
	and d
	ld e, a
	WAIT_FOR_HBLANK
	ld [hl], e
	inc hl
	ld a, h
	cp a, $8F
	jr nz, .dontSkipAhead
	ld hl, $9690
.dontSkipAhead
	rrc d				; rotate the bitmask to the right
	dec bc
	ld a, c
	or b
	jr nz, .maskTileRow
	ldh a, [$FFFC]
	sla a				; shift a new zero bit in the mask
	jr z, .allTilesGone
	swap a
	ldh [$FFFC], a
	ld a, $3F
	ldh [hTimer], a
	ret

.allTilesGone
	xor a
	ld [wScrollY], a		; stop screen shake
	ld [$C0D2], a
	inc a
	ldh [hIsUnderground], a
	ld hl, hGameState
	inc [hl]			; 27 → 28
	ret

GameState_29:: ; 1116
	di
	xor a
	ldh [rLCDC], a
	ldh [hIsUnderground], a
	ld hl, vBGMap1
	ld bc, $0100
	call EraseTileMap
	call DrawInitialScreen.drawLevel
	call HandleMarioWalksOffscreen.prepareMarioAndDaisy
	ld hl, wMarioPosX		; mario X position
	ld [hl], 56
	inc l
	ld [hl], $10		; Super Mario
	ld hl, wEntityPosX
	ld [hl], 120		; Daisy X position
	xor a
	ldh [rIF], a
	ldh [hScrollX], a
	ld [wScrollY], a
	ldh [$FFFB], a
	ld hl, wOAMBuffer
	ld b, 3 * 4			; 3 projectiles?
.loop
	ldi [hl], a
	dec b
	jr nz, .loop
	call Call_1736
	ld a, HIGH(vBGMap0) ; $98
	ldh [hTextCursorHi], a
	ld a, $A5
	ldh [hTextCursorLo], a
	ld a, $0F
	ld [wActiveMusic], a
	ld a, %11000011
	ldh [rLCDC], a
	ei
	ld hl, hGameState
	inc [hl]			; 29 → 2A
	ret

HandleDaisySpeaking:: ; 1165
	ld hl, OhDaisyText
	call PrintVictoryMessage
	cp a, "@"
	ret nz
	xor a
	ldh [$FFFB], a
	ld a, $99
	ldh [hTextCursorHi], a
	ld a, 2
	ldh [hTextCursorLo], a
	ld a, $23
	ld [wEntityAnimIndex], a		; animation index?
	ld hl, hGameState
	inc [hl]						; 2A → 2B
	ret

OhDaisyText::
	db "oh! daisy<LINE>", $1B, "daisy@"

; Daisy running towards Mario
HandleDaisyMovingTowardsMario:: ; 1194
	ld hl, ThankYouMarioText
	call PrintVictoryMessage
	ldh a, [hFrameCounter]
	and a, 3
	ret nz
	ld hl, wEntityPosX		; daisy X pos
	ld a, [hl]
	cp a, $44
	jr c, .out
	dec [hl]
	call Call_1736
	ret

.out
	ld hl, hGameState
	inc [hl]
	ld hl, wOAMBuffer +  4 * $C
	ld [hl], 112		; Y pos
	inc l
	ld [hl], 58		    ; X pos
	inc l
	ld [hl], "♥"		; sprite
	inc l
	ld [hl], 0
	ret

ThankYouMarioText::
	db "thank you mario.@"

; kiss ^_^
HandleDaisyKiss:: ; 11D0
	ldh a, [hFrameCounter]
	and a, 1
	ret nz
	ld hl, wOAMBuffer + 4 * $C ; todo object macro?
	dec [hl]				; Y pos
	ldi a, [hl]
	cp a, SCRN_VY_B			; if heart gets high
	jr c, .clearMessageAndOut
	ldh a, [$FFFB]
	and a
	ld a, [hl]
	jr nz, .goRight
	dec [hl]				; heart goes left
	cp a, $30
	ret nc
.out
	ldh [$FFFB], a
	ret

.goRight
	inc [hl]
	cp a, $50
	ret c
	xor a
	jr .out

.clearMessageAndOut
	ld [hl], $F0
	ld b, $6D
	ld hl, vBGMap0 + 5 * SCRN_VX_B + 5 ; $98A5
.loop
	WAIT_FOR_HBLANK
	WAIT_FOR_HBLANK
	ld [hl], " "
	inc hl
	dec b
	jr nz, .loop
	xor a
	ldh [$FFFB], a
	ld a, $99
	ldh [hTextCursorHi], a
	ld a, 0
	ldh [hTextCursorLo], a
	ld hl, hGameState
	inc [hl]			; 2C → 2D
	ret

; your quest is over
HandleDaisyQuestOver:: ; 121B
	ld hl, QuestOverText
	call PrintVictoryMessage
	cp a, "@"
	ret nz
	ld hl, wEntityAnimIndex	; daisy run
	ld [hl], $24
	inc l
	inc l
	ld [hl], $00	; facing right?
	ld hl, $C241	; spaceship entity
	ld [hl], $7E	; Y pos
	inc l
	inc l
	ld [hl], $28	; spaceship
	inc l
	inc l
	ld [hl], $00	; facing right
	ld hl, hGameState
	inc [hl]		; 2D → 2E
	ret

QuestOverText::
	db "-your quest is over-@"

; Mario & Daisy walking
HandleMarioAndDaisyWalking::
	ldh a, [hFrameCounter]
	and a, 3
	jr nz, .jmp_1261
	ld hl, wEntityAnimIndex		; daisy animation
	ld a, [hl]
	xor a, 1			; two daisy walking objects
	ld [hl], a
.jmp_1261
	ld hl, $C240		; spaceship
	ld a, [hl]
	and a
	jr nz, .walkMarioDaisy
	inc l				; move spaceship
	inc l
	dec [hl]			; X pos
	ld a, [hl]
	cp a, 80
	jr nz, .checkIfBothAreInSpaceship	; todo name
	ld a, SPRITE_HIDDEN			; Mario "enters" the spaceship (becomes invisible)
	ld [wMarioVisible], a
	jr .walkMarioDaisy

.checkIfBothAreInSpaceship
	cp a, $40
	jr nz, .walkMarioDaisy
	ld a, SPRITE_HIDDEN
	ld [wEntityVisible], a		; Daisy "enters" the spaceship
	ld a, $40
	ldh [hTimer], a		; 40 frames, 2/3 second
	ld hl, hGameState
	inc [hl]			; 2E → 2F
.walkMarioDaisy
	call MarioStartsWalkingOffscreen.walkRight
	call LoadColumns		; level rendering
	ldh a, [hScreenIndex]
	cp a, 3
	ret nz
	ldh a, [hColumnIndex]
	and a
	ret nz
	ld hl, $C240		; spaceship
	ld [hl], 0			; make visible?
	inc l
	inc l
	ld [hl], 192		; X pos
	ret

; prepare for liftoff
HandleEnterAirplane:: ; 12A1
	ldh a, [hTimer]
	and a
	ret nz
	ld hl, $C240		; spaceship
	ld de, $C200		; mario
	ld b, 6
.loop
	ldi a, [hl]
	ld [de], a
	inc e
	dec b
	jr nz, .loop		; todo macro?
	ld hl, wAnimIndex		; animation
	ld [hl], $26
	ld hl, $C241		; previous spaceship
	ld [hl], $F0		; out of sight, out of mind
	ld hl, hGameState
	inc [hl]			; 2F → 30
	ret

HandleAirplaneTakingOff:: ; 12C2
	call Call_1736			; animate "Mario" (spaceship)
	ldh a, [hFrameCounter]
	ld b, a
	and a, 1
	ret nz
	ld hl, $C240
	ld [hl], $FF		; make invisible. Why not do this before?
	ld hl, wMarioPosY		; Y pos
	dec [hl]				; take off
	ldi a, [hl]
	cp a, $58
	jr z, .cruisingAltitude
	call .switchSpaceshipAnimation
	ret

.cruisingAltitude
	ld hl, hGameState
	inc [hl]				; 30 → 31
	ld a, 4
	ldh [$FFFB], a
	ret

.switchSpaceshipAnimation
	ldh a, [hFrameCounter]
	and a, 3
	ret nz
	inc l
	ld a, [hl]				; C203, animation
	xor a, 1				; switch exhaust flame
	ld [hl], a
	ret

HandleAirplaneMovingForward:: ; 12F1
	call .animateSpaceship
	call LoadColumns		; loads level
	ldh a, [hScrollX]
	inc a
	call z, .call_1318
	inc a
	call z, .call_1318
	ldh [hScrollX], a
	ld a, [$DFE9]		; wait until the song is over?
	and a
	ret nz
	ld a, $11
	ld [wActiveMusic], a
	ret

.animateSpaceship
	ld hl, wMarioPosX		; X pos
	call HandleAirplaneTakingOff.switchSpaceshipAnimation
	call Call_1736		; animate entities
	ret

.call_1318
	push af
	ldh a, [$FFFB]
	dec a				; FFFB starts at 4. So traverse 4 blocks
	ld [$FFFB], a
	jr nz, .out
	ldh [rLYC], a		; A is 0 here. Removes HUD?
	ld a, $21
	ldh [$FFFB], a
	ld a, $54
	ldh [$FFE9], a
	call .clearColumn
	ld hl, $C210
	ld de, .data_137F
	call .replaceEntity
	ld hl, $C220		; cloud?
	ld de, .data_1384
	call .replaceEntity
	ld hl, $C230
	ld de, .data_1389
	call .replaceEntity
	ld hl, hGameState
	inc [hl]
.out
	pop af
	ret

.clearColumn
	ld hl, $C0B0
	ld b, $10
	ld a, $2C
.clearLoop
	ldi [hl], a
	dec b
	jr nz, .clearLoop
	ld a , 1
	ldh [$FFEA], a
	ld b, 2
	ldh a, [$FFE9]		; first not yet loaded column
	sub a, $20
	ld l, a
	ld h, HIGH(vBGMap0) ; $98
.clearLoop2
	WAIT_FOR_HBLANK
	ld [hl], " "
	ld a, l
	sub a, $20
	ld l, a
	dec b
	jr nz, .clearLoop2
	ret

.replaceEntity
	ld b, 5
.loop
	ld a, [de]
	ldi [hl], a
	inc de
	dec b
	jr nz, .loop
	ret

; some sort of sprite data
.data_137F
	db $00, $30, $D0, $29, $80

.data_1384
	db $80, $70, $10, $2A, $80

.data_1389
	db $80, $40, $70, $29, $80

HandleAirplaneLeavingHanger:: ; 138E
	call AnimateSpaceshipAndClouds
	ldh a, [hScrollX]
	inc a
	inc a
	ldh [hScrollX], a
	and a, 8
	ld b, a
	ldh a, [$FFA3]	; switches between 0 and 8, depending on column loaded
	cp b
	ret nz
	xor a, 8
	ldh [$FFA3], a
	call HandleAirplaneMovingForward.clearColumn
	ldh a, [$FFFB]
	dec a
	ldh [$FFFB], a
	ret nz
	xor a
	ldh [hScrollX], a
	ld a, $60
	ldh [rLYC], a
	ld hl, CreditsText
	ld a, h
	ldh [hTextCursorHi], a
	ld a, l
	ldh [hTextCursorLo], a
	ld a, $F0
	ldh [hTimer], a
	ld hl, hGameState
	inc [hl]		; 32 → 33
	ret


GameState_33:: ; 13C4
.animateClouds
	ld hl, wEntityPosX	; clouds X pos
	ld de, $0010
	ld b, 3
.floatCloud
	dec [hl]		; float to the left
	ld a, [hl]
	cp a, 1
	jr nz, .checkCloudForReset
	ld [hl], $FE
	jr .nextCloud

.checkCloudForReset
	cp a, $E0
	jr nz, .nextCloud	; reset if cloud hits E0 from the right
	push hl
	ldh a, [rDIV]	; divider register, pseudorandom
	dec l			; Y position
	add [hl]
	and a, $7F		; make Y pos <= 7F
	cp a, $68		; clear between 3F and 68 (spaceship, the end)
	jr nc, .resetCloud
	and a, $3F
.resetCloud
	ldd [hl], a
	ld [hl], 0
	pop hl
.nextCloud
	add hl, de
	dec b
	jr nz, .floatCloud
	ret

.entryPoint:: ; 13F0
	call AnimateSpaceshipAndClouds
	ldh a, [hTimer]
	and a
	ret nz
	ldh a, [hTextCursorHi]
	ld h, a
	ldh a, [hTextCursorLo]
	ld l, a
	ld de, $9A42	; start of first line. Below the stage, scrolled in later
.printLine
	ld a, [hl]
	cp a, "<LINE>"		; end of line
	jr z, .eraseTillEndOfLine
	inc hl
	ld b, a
.printCharacter
	WAIT_FOR_HBLANK
	WAIT_FOR_HBLANK
	ld a, b
	ld [de], a
	inc de
	ld a, e
	cp a, $54
	jr z, .nextLine
	cp a, $93
	jr z, .startCreditsScroll
	jr .printLine

.eraseTillEndOfLine
	ld b, " "
	jr .printCharacter

.nextLine
	ld de, $9A87
	inc hl
	jr .printLine

.startCreditsScroll
	inc hl
	ld a, [hl]
	cp a, $FF
	jr nz, .nextState
	ld a, $FF
	ld [$C0DE], a
.nextState
	ld a, h
	ldh [hTextCursorHi], a
	ld a, l
	ldh [hTextCursorLo], a
	ld hl, hGameState
	inc [hl]			; 33 → 34
	ret

; credits entering
HandleCreditsComingUp::
	call AnimateSpaceshipAndClouds
	ldh a, [hFrameCounter]
	and a, 3
	ret nz
	ld hl, wScrollY
	inc [hl]
	ld a, [hl]
	cp a, $20			; scroll $20 pxs up
	ret nz
	ld hl, hGameState
	inc [hl]			; 34 → 35
	ld a, $50
	ldh [hTimer], a
	ret


HandleCreditsStandingStill:: ; 145A
	call AnimateSpaceshipAndClouds
	ldh a, [hTimer]
	and a
	ret nz
	ld hl, hGameState
	inc [hl]			; 35 → 36
	ret

; scroll credits up, out of sight
HandleCreditsLeave:: ; 1466
	call AnimateSpaceshipAndClouds
	ldh a, [hFrameCounter]
	and a, 3
	ret nz
	ld hl, wScrollY
	inc [hl]
	ld a, [hl]
	cp a, $50
	ret nz
	xor a
	ld [wScrollY], a
	ld a, [$C0DE]
	cp a, $FF
	ld a, $33
	jr nz, .out
	ld a, $37
.out
	ldh [hGameState], a
	ret

; spaceship flies off, prepare "THE END"
HandleAirplaneLeaving::
	call AnimateSpaceshipAndClouds
	ld hl, wMarioPosX		; X position
	inc [hl]
	ld a, [hl]
	cp a, $D0			; out of sight??
	ret nz
	dec l
	ld [hl], $F0
	push hl
	call Call_1736
	pop hl
	dec l
	ld [hl], $FF			; make invisible?
	ld hl, wOAMBuffer + 4 * $1C	; object 1C?
	ld de, TheEndText
	ld b, TheEndTextEnd- TheEndText
.loop1
	ld a, [de]
	ldi [hl], a
	inc de
	dec b
	jr nz, .loop1
	ld b, $18
	xor a
.loop2
	ldi [hl], a
	dec b
	jr nz, .loop2
	ld a, $90
	ldh [hTimer], a
	ldh a, [hWinCount]
	inc a
	ldh [hWinCount], a
	ld [wWinCount], a
	ld hl, hGameState
	inc [hl]			; 37 → 38
	ret

TheEndText::
	db $4E, $CC, $52, 00 ; T
	db $4E, $D4, $53, 00 ; H
	db $4E, $DC, $54, 00 ; E
	db $4E, $EC, $54, 00 ; E
	db $4E, $F4, $55, 00 ; N
	db $4E, $FC, $56, 00 ; D
TheEndTextEnd:


HandleTheEnd::
	call AnimateSpaceshipAndClouds
	ldh a, [hTimer]
	and a
	ret nz
	ld hl, $C071		; letter object X position
	ld a, [hl]
	cp a, $3C
	jr z, .nextLetter
.animateLetter
	dec [hl]
	dec [hl]
	dec [hl]
	ret

.nextLetter
	ld hl, $C075
	ld a, [hl]
	cp a, $44
	jr nz, .animateLetter
	ld hl, $C079
	ld a, [hl]
	cp a, $4C
	jr nz, .animateLetter
	ld hl, $C07D
	ld a, [hl]
	cp a, $5C
	jr nz, .animateLetter
	ld hl, $C081
	ld a, [hl]
	cp a, $64
	jr nz, .animateLetter
	ld hl, $C085
	ld a, [hl]
	cp a, $6C
	jr nz, .animateLetter
	call .checkForButtonPress
	xor a
	ldh [hLevelIndex], a
	ldh [hSuperStatus], a
	ldh [hSuperballMario], a
	ld [wNumContinues], a
	ld a, $11
	ldh [hWorldAndLevel], a
	ret

.checkForButtonPress
	ldh a, [hJoyPressed]
	and a
	ret z
	call InitSound
.resetToMenu
	ld a, 2
	ldh [hActiveRomBank], a
	ld [MBC1RomBank], a
	ld [$C0DC], a
	ld [$C0A4], a
	xor a
	ld [wGameTimer], a
	ld [wGameOverWindowEnabled], a
	ld [wGameOverTimerExpired], a
	ld a, 3
	ldh [rIE], a
	ld a, STATE_LOAD_MENU
	ldh [hGameState], a	; init menu
	ret

AnimateSpaceshipAndClouds
	call HandleAirplaneMovingForward.animateSpaceship
	call GameState_33.animateClouds
	ret

CreditsText
	db "producer<LINE>"
	db "g.yokoi<LINE>"
	db "director<LINE>"
	db "s.okada<LINE>"
	db "programmer<LINE>"
	db "m.yamamoto<LINE>"
	db "programmer<LINE>"
	db "t.harada<LINE>"
	db "design<LINE>"
	db "h.matsuoka<LINE>"
	db "sound<LINE>"
	db "h.tanaka<LINE>"
	db "amida<LINE>"
	db "m.yamanaka<LINE>"
	db "design<LINE>"
	db "mashimo<LINE>"
	db "special thanks to:<LINE>"
	db "taki<LINE>"
	db "izushi<LINE>"
	db "nagata<LINE>"
	db "kanoh<LINE>"
	db "nishizawa<LINE>"
	db "@"

; go down pipe
HandleGoingDownPipe:: ; 161B
	ld hl, wMarioPosY		; Mario Y position
	ldh a, [$FFF8]		; Y position of block under pipe? Y target at least
	cp [hl]
	jr z, .toUnderground
	inc [hl]			; Y coord increases going down
	call Call_16F5		; animate or so
	ret
.toUnderground
	ld a, STATE_WARP_TO_UNDERGROUND
	ldh [hGameState], a	; warp to underground
	ldh [hIsUnderground], a
	ret

; warp to underground
HandleUndergroundWarping:: ; 162F
	di
	xor a
	ldh [rLCDC], a
	ldh [hColumnIndex], a
	call ClearSprites		; clears objects that aren't player, enemy or platform?
	call ClearOverlay
	ldh a, [$FFF4]
	ldh [hScreenIndex], a
	call DrawInitialScreen		; draws the first screen of the "level"
	call InitEnemySlots
	ld hl, wMarioPosY		; Mario Y position
	ld [hl], 32				; up high
	inc l					; Mario X position
	ld [hl], 29 			; a little to the left
	inc l
	inc l
	ld [hl], $00		; direction mario is facing
	xor a
	ldh [rIF], a
	ldh [hGameState], a
	ldh [hScrollX], a
	ld a, %11000011			; todo
	ld [rLCDC], a
	ei
	ret

; clear some sort of overlay?
ClearOverlay ; 165E
	ld hl, $CA3F		; the bottom rightmost tile of the total area
	ld bc, $0240
.clearLoop
	xor a
	ldd [hl], a
	dec bc
	ld a, b
	or c
	jr nz, .clearLoop	; you've got to be fucking kidding me
	ret

; going in pipe out of underground
HandleGoingRightIntoPipe:: ; 166C
	ldh a, [hFrameCounter]
	and a, 1			; slow down mario by half
	ret z
	ld hl, wMarioPosX		; screen X position
	ldh a, [$FFF8]			; goal X value?
	cp [hl]
	jr c, .toOverworld		; warp out? todo
	inc [hl]
	ld hl, $C20B			; how many frames a direction is held. For animation?
	inc [hl]
	call Call_16F5			; animate mario?
	ret

.toOverworld
	di
	ldh a, [$FFF5]		; (one less than) the screen we went in?
	ldh [hScreenIndex], a
	xor a
	ldh [rLCDC], a		; turn off lcd
	ldh [hColumnIndex], a
	call ClearOverlay	; A is zero after this
	ld hl, $FFF4
	ldi [hl], a
	ldi [hl], a
	ldh a, [$FFF7]		; FFF6 and FFF7 contain the coords where Mario will
	ld d, a				; come out of the pipe
	ldh a, [$FFF6]
	ld e, a
	push de
	call DrawInitialScreen		; draw the level
	pop de
	ld a, $80
	ld [wC204], a
	ld hl, wMarioPosY		; mario Y
	ld a, d
	ldi [hl], a
	sub a, $12			; target position is 12 units above spawn? right on top
	ldh [$FFF8], a		; of pipe?
	ld a, e
	ld [hl], a
	ldh a, [hScreenIndex]
	sub a, 4
	ld b, a
	rlca
	rlca
	rlca
	add b
	add b
	add a, $0C			; a = (hScreenIndex - 4)* 10 + 0xC....?
	ld [$C0AB], a		; sort of progress in the level in columns / 2?
	xor a
	ldh [rIF], a
	ldh [hScrollX], a
	ld a, $5B
	ldh [$FFE9], a		; first col not yet loaded in. IMO $807 should do this
	call InitEnemySlots
	call ClearSprites		; clears objects
	ld a, $C3
	ldh [rLCDC], a
	ld a, STATE_12
	ldh [hGameState], a
	call StartLevelMusic
	ei
	ret

; coming up out of pipe
HandleGoingUpOutOfPipe:: ; 16DA
	ldh a, [hFrameCounter]
	and a, $01				; slow down animation by 2
	ret z
	ld hl, wMarioPosY			; y position
	ldh a, [$FFF8]			; y target?
	cp [hl]
	jr z, .outOfPipe
	dec [hl]				; Y coordinate decrease going up
	call Call_16F5			; animate?
	ret
.outOfPipe
	xor a
	ldh [hGameState], a
	ld [wC204], a			; smth to do with mario having control?
	ldh [hIsUnderground], a
	ret

Call_16F5:: ; 16F5 Animate mario?
	call Call_1736
	ld a, [wMarioOnGround]			; 1 if mario on the ground
	and a
	jr z, .jmp_172C
	ld a, [wAnimIndex]			; animation index
	and a, $0F				; low nibble
	cp a, $0A
	jr nc, .jmp_172C		; JR if animation index is >= 0xA, which is sub and airplane stuff
	ld hl, $C20B			; animation frame counter?
	ld a, [$C20E]			; 2 when walking, 4 when stuff
	cp a, $23
	ld a, [hl]
	jr z, .jmp_1730			; wait, can this ever happen... Bug?
	and a, 3
	jr nz, .jmp_172C		; any 3 movement frames, change animation
.jmp_1716
	ld hl, wAnimIndex
	ld a, [hl]
	cp a, $18				; crouching Super Mario
	jr z, .jmp_172C
	inc [hl]
	ld a, [hl]
	and a, $0F
	cp a, 4				; 3 sprites in the walking animation
	jr c, .jmp_172C
	ld a, [hl]
	and a, $F0
	or a, 1
	ld [hl], a
.jmp_172C
	call MoveMario			; check movement keys, move mario?
	ret
.jmp_1730
	and a, 1
	jr nz, .jmp_172C
	jr .jmp_1716

; animate mario?
Call_1736::
	ld a, $0C
	ldh [$FF8E], a	; oh joy, new variables..
	ld hl, $C200
	ld a, $C0
	ldh [$FF8D], a
	ld a, 5
	ldh [hBoundingBoxRight], a
	homecall Call_4823
	ret

; standing on boss switch
MarioStandingOnBossSwitch:: ; 175B
	ldh a, [hGameState]
	cp a, STATE_LOAD_MENU
	jp nc, Call_17BC.jmp_181E
	jp Jmp_1B45				; Mario wins

; Called every frame when standing on a pipe?
MarioStandingOnPipe:: ; 1765
	ldh a, [hJoyHeld]
	bit D_DOWN_BIT, a
	jp z, Jmp_185D			; Down button
	ld bc, -$20				; one screen width?
	ld a, h
	ldh [$FFB0], a
	ld a, l
	ldh [$FFAF], a
	ld a, h
	add a, $30
	ld h, a
	ld de, $FFF4
	ld a, [hl]
	and a
	jp z, Jmp_185D
	ld [de], a
	inc e
	add hl, bc
	ld a, [hl]
	ld [de], a
	inc e
	add hl, bc
	ld a, [hl]
	ld [de], a
	inc e
	add hl, bc
	ld a, [hl]
	ld [de], a
	inc e
	push de
	call Call_3F13			; called when hitting a bouncing block
	pop de
	ld hl, wMarioPosY			; Y pos
	ldi a, [hl]
	add a, 16
	ld [de], a
	ldh a, [hScrollX]
	ld b, a
	ldh a, [$FFAE]
	sub b
	add a, 8
	ldi [hl], a
	inc l
	ld [hl], $80
	ld a, STATE_GOING_DOWN_PIPE
	ldh [hGameState], a		; go down pipe
	ld a, [wInvincibilityTimer]
	and a
	jr nz, .skip			; Keep invincibility music going
	ld a, 4
	ld [wActiveMusic], a			; underground music
.skip
	call ClearSprites			; clears some sprites
	jp Jmp_185D

; called every frame?
Call_17BC:: ; 17BC
	ld hl, wJumpStatus			; jump status
	ld a, [hl]
	cp a, 1
	ret z
	ld hl, wMarioPosY			; Y pos
	ldi a, [hl]
	add a, $0B
	ldh [$FFAD], a
	ldh a, [hScrollX]
	ld b, a
	ld a, [hl]
	add b
	add a, -2				; -2?
	ldh [$FFAE], a
	call LookupTile
	cp a, BLOCK_PIPE_OPENING		; standing on pipe
	jr z, MarioStandingOnPipe
	cp a, $E1				; boss switch
	jp z, MarioStandingOnBossSwitch			; can this be a JR?
	cp a, $60				; solid tiles
	jr nc, .jmp_181E		; why is this a JP? Bug?
	ld a, [$C20E]			; 02 walking, 04 running
	ld b, 4
	cp a, 4
	jr nz, .jmp_17F5
	ld a, [wJumpStatus]			; jump status
	and a
	jr nz, .jmp_17F5
	ld b, $08
.jmp_17F5
	ldh a, [$FFAE]
	add b
	ldh [$FFAE], a
	call LookupTile
	cp a, $60
	jr nc, .jmp_181E
.jmp_1801
	ld hl, wJumpStatus
	ld a, [hl]
	cp a, 2
	ret z					; return if descending
	ld hl, wMarioPosY			; Y pos
	inc [hl]
	inc [hl]
	inc [hl]				; falling without having jumped
	ld hl, wMarioOnGround
	ld [hl], 0				; Mario not on ground
	ld a, [$C20E]
	and a
	ret nz
	ld a, 2
	ld [$C20E], a
	ret

.jmp_181E
	cp a, $ED				; spike
	push af
	jr nz, .jmp_1842
	ld a, [wInvincibilityTimer]
	and a
	jr nz, .jmp_1842
	ldh a, [hSuperStatus]
	and a
	jr z, .jmp_183C
	cp a, $04				; i frames after hit
	jr z, .jmp_1842
	cp a, $02
	jr nz, .jmp_1842
	pop af
	call InjureMario
	jr Jmp_185D

.jmp_183C
	pop af
	call KillMario
	jr Jmp_185D

.jmp_1842
	pop af
	cp a, $F4				; Coin
	jr nz, Jmp_185D
	push hl
	pop de
	ld hl, $FFEE
	ld a, [hl]
	and a
	jr nz, .jmp_1801
	ld [hl], $C0
	inc l
	ld [hl], d
	inc l
	ld [hl], e
	ld a, SOUND_COIN
	ld [wSquareSFX], a			; coin sound
	jr .jmp_1801

Jmp_185D
	ld hl, wMarioPosY
	ld a, [hl]
	dec a
	dec a
	and a, $FC				; erase 0000 0011
	or a, 6				; set   0000 0110
	ld [hl], a
	xor a
	ld hl, wJumpStatus		; jump status
	ldi [hl], a				; C207 jump status
	ldi [hl], a				; C208 
	ldi [hl], a				; C209
	ld [hl], 1				; C20A 1 if mario on the ground
	ld hl, wMarioMomentum			; C20C Momentum
	ld a, [hl]
	cp a, 7
	ret c
	ld [hl], 6
	ret

; hidden block
.jmp_187B:: ; 187B
	ldh a, [$FFEE]
	and a
	ret nz
	push hl					; HL contains VRAM location of hidden block
	ld a, h
	add a, $30				; overlay? :/
	ld h, a
	ld a, [hl]
	pop hl
	and a					; if 0, don't make the hidden block appear
	ret z					; it's from a different level screen/block
.jmp_1888
	ldh a, [$FFEE]
	and a
	ret nz
	push hl
	ld a, h
	add a, $30
	ld h, a
	ld a, [hl]				; wait, why again?
	pop hl
	and a
	jp z, .hitBreakableBlock
	cp a, $F0				; nothing, just a solid grey block
	jr z, .jmp_18C0
.jmp_189B
	cp a, $C0				; coin block
	jr nz, .jmp_18C7		; everything else comes out of a solid grey block
	ld a, $FF
	ld [$C0CE], a			; timer :)
.jmp_18A4
	ldh a, [$FFEE]
	and a
	ret nz
	ld a, SOUND_COIN
	ld [wSquareSFX], a			; coin sound effect
	ld a, [wMarioPosY]			; Y pos
	sub a, 16
	ldh [hFloatyY], a
	ld a, $C0
	ldh [hFloatyControl], a
	ldh [$FFFE], a
	ld a, [$C0CE]
	and a
	jr nz, .jmp_1923
.jmp_18C0
	ld a, $80
	ld [wOAMBuffer + 4 * $B + 2], a
	jr .jmp_1937

.jmp_18C7
	ldh [hBoundingBoxTop], a
	ld a, $80
	ld [wOAMBuffer + 4 * $B + 2], a
	ld a, SOUND_BUMP
	ld [wSquareSFX], a			; bump sound effect
	push hl
	pop de
	ld hl, $FFEE
	ld a, [hl]
	and a
	ret nz
	ld [hl], $02			; bounce block?
	inc l
	ld [hl], d
	inc l
	ld [hl], e
	ld a, d
	ldh [$FFB0], a
	ld a, e
	ldh [$FFAF], a
	ld a, d
	add a, $30
	ld d, a
	ld a, [de]
	ldh [hBoundingBoxTop], a
	call Call_3F13
	ld hl, wOAMBuffer + 4 * $B
	ld a, [wMarioPosY]
	sub a, $0B
	ldi [hl], a				; Y pos of sprite on top of mario?
	ldh [$FFC2], a			; enemy Y pos buffer?
	ldh [$FFF1], a
	ldh a, [hScrollX]
	ld b, a
	ldh a, [$FFAE]
	ldh [$FFF2], a
	sub b
	ldi [hl], a
	ldh [$FFC3], a			; enemy X pos buffer?
	inc l
	ld [hl], $00
	ldh a, [hBoundingBoxTop]
	cp a, $F0
	ret z
	cp a, $28				; mushroom
	jr nz, .jmp_191F
	ldh a, [hSuperStatus]
	cp a, 2
	ld a, $28
	jr nz, .jmp_191F
	ld a, $2D				; flower
.jmp_191F
	call Call_254D				; make it come out?
	ret

.jmp_1923
	ldh a, [$FFEE]
	and a
	ret nz
	ld a, $82
	ld [$C02E], a
	ld a, [wSquareSFX]
	and a
	jr nz, .jmp_1937
	ld a, $07
	ld [wSquareSFX], a

.jmp_1937
	push hl
	pop de
	ld hl, $FFEE
	ld [hl], $02
	inc l
	ld [hl], d
	inc l
	ld [hl], e
	ld a, d
	ldh [$FFB0], a
	ld a, e					; store HL in FFEF, FFF0, FFB0 and FFAF?
	ld [$FFAF], a			; what a mess
	call Call_3F13
	ld hl, wOAMBuffer + 4 * $B
	ld a, [wMarioPosY]			; y pos
	sub a, $B
	ldi [hl], a
	ldh [$FFF1], a
	ldh a, [hScrollX]
	ld b, a
	ld a, [$FFAE]
	ld c, a
	ldh [$FFF2], a
	sub b
	ldi [hl], a
	inc l
	ld [hl], $00
	ldh [hFloatyX], a		; why
	ret

; hitting a Mystery Block
.hitMysteryBlock:: ; 1966
	ldh a, [$FFEE]
	and a
	ret nz
	push hl
	ld a, h
	add a, $30
	ld h, a
	ld a, [hl]
	pop hl
	and a
	jp nz, .jmp_189B
	ld a, 5					; empty Mystery Block contain a single coin
	ld [wSquareSFX], a
	ld a, $81
	ld [$C02E], a
	ld a, [wMarioPosY]			; ypos
	sub a, 16
	ldh [hFloatyY], a
	ld a, $C0
	ldh [hFloatyControl], a
	jr .jmp_1937

.call_198C
	ld a, [wJumpStatus]			; jump status
	cp a, MARIO_ASCENDING		; ascending
	ret nz
	ld hl, wMarioPosY
	ldi a, [hl]
	add a, -3				; look for collision on mario's head
	ldh [$FFAD], a
	ldh a, [hScrollX]
	ld b, [hl]
	add b
	add a, 2
	ldh [$FFAE], a
	call LookupTile
	cp a, $5F				; Hidden Block
	jp z, .jmp_187B
	cp a, $60
	jr nc, .jmp_19BF
	ldh a, [$FFAE]
	add a, -$4
	ldh [$FFAE], a
	call LookupTile
	cp a, $5F
	jp z, .jmp_187B
	cp a, $60
	ret c					; non solid block
.jmp_19BF
	call Call_1A6B			; platform-like blocks
	and a
	ret z
	cp a, $82				; breakable block
	jr z, .hitBreakableBlock
	cp a, $F4				; coin
	jp z, .jmp_1A57
	cp a, BLOCK_COIN				; mystery block
	jr z, .hitMysteryBlock
	cp a, BLOCK_BREAKABLE
	jp z, .jmp_1888
	ld a, 2
	ld [wJumpStatus], a
	ld a, 7
	ld [wSquareSFX], a			; bump
	ret

.hitBreakableBlock
	push hl
	ld a, h
	add a, $30
	ld h, a
	ld a, [hl]
	pop hl
	cp a, $C0
	jp z, .jmp_18A4
	ldh a, [hSuperStatus]
	cp a, 2
	jp nz, .jmp_1923
	push hl
	pop de
	ld hl, $FFEE
	ld a, [hl]
	and a
	ret nz
	ld [hl], 1
	inc l
	ld [hl], d
	inc l
	ld [hl], e
	ld hl, $C210			; block fragments?
	ld de, $0010
	ld b, 4
.jmp_1A0A
	push hl
	ld [hl], 0
	inc l					; C2x1 Y
	ld a, [wMarioPosY]
	add a, -$D
	ld [hl], a
	inc l					; C2x2 X
	ld a, [wMarioPosX]
	add a, $2
	ld [hl], a
	inc l					; C2x3
	inc l                   ; C2x4
	inc l                   ; C2x5
	inc l                   ; C2x6
	inc l                   ; C2x7
	ld [hl], 1
	inc l					; C2x8
	ld [hl], 7
	pop hl
	add hl, de
	dec b
	jr nz, .jmp_1A0A
	ld hl, $C222
	ld a, [hl]
	sub a, 4
	ld [hl], a
	ld hl, $C242
	ld a, [hl]
	sub a, 4
	ld [hl], a
	ld hl, $C238
	ld [hl], $0B
	ld hl, $C248
	ld [hl], $0B
	ldh a, [hScrollX]
	ldh [$FFF3], a
	ld a, SOUND_BLOCK_BREAKING
	ld [wNoiseSFX], a			; breaking block sound effect
	ld de, $0050
	call AddScore
	ld a, 2
	ld [wJumpStatus], a
	ret

.jmp_1A57
	push hl
	pop de
	ld hl, $FFEE
	ld a, [hl]
	and a
	ret nz
	ld [hl], $C0
	inc l
	ld [hl], d
	inc l
	ld [hl], e
	ld a, 5
	ld [wSquareSFX], a
	ret

; Clears A if it's a solid block that does not have side or top collision,
; but can be stood upon, like a platform. Semi-solid platform
Call_1A6B:: ; 1A6B
	push hl
	push af
	ld b, a
	ldh a, [hWorldAndLevel]
	and a, $F0
	swap a					; just the world
	dec a
	sla a					; times two
	ld e, a
	ld d, 0
	ld hl, .data_1A93		; lookup in table
	add hl, de
	ld e, [hl]
	inc hl
	ld d, [hl]				; load the address in DE
.nextBlock
	ld a, [de]
	cp a, $FD
	jr z, .endOfList
	cp b
	jr z, .match
	inc de
	jr .nextBlock

.endOfList					; no match, don't clear A
	pop af
	pop hl
	ret

.match						; clear A, no collision
	pop af
	pop hl
	xor a
	ret

.data_1A93
	dw .TreeTiles
	dw .GrassTiles
	dw .data_1AA7
	dw .data_1AA9
	dw .data_1AAB

.TreeTiles
	db $68, $69, $6A, $7C, $FD		; "trees" in 1-2 , 7C never seems to appear
.GrassTiles
	db $60, $61, $63, $7C, $FD		; "mountains" in 2-1
.data_1AA7
	db $7C, $FD
.data_1AA9
	db $7C, $FD
.data_1AAB
	db $7C, $FD						; ???? World 5?? Bug?

; detects collision with environment, but only left and right?
Call_1AAD:: ; 1AAD
	ldh a, [hGameState]
	cp a, $0E
	jr nc, .noCollision		; jump if not in "normal gameplay"
	ld de, $0701
	ldh a, [hSuperStatus]
	cp a, 2					; super mario
	jr nz, .checkSide
	ld a, [wAnimIndex]		; animation index
	cp a, $18				; crouching mario
	jr z, .checkSide
	ld de, $0702			; E = 2, check lower and upper side
.checkSide
	ld hl, wMarioPosY			; Y pos
	ldi a, [hl] 			; The Y pos is about 10 px above Mario's feet
	add d					; so look about 7px lower first iteration?
	ldh [$FFAD], a
	ld a, [wMarioFacingDir]			; dir facing
	ld b, [hl]				; X pos
	ld c, -6				; mario is 12 pixels wide?
	and a
	jr nz, .findTile
	ld c, 6					; 6 pixels to the right if he's facing right
.findTile					; Bug? Mario's X pos is one pixel to the right of 
	ld a, c					; his center, so adding 6 is wrong, asymmetric
	add b					; with subtracting 6
	ld b, a
	ldh a, [hScrollX]
	add b					; add to find X coordinate in tile coordinates
	ldh [$FFAE], a			; used in block detection
	push de
	call LookupTile
	call Call_1A6B			; detect if the block is passthrougable from the side
	pop de
	and a
	jr z, .checkNextTile
	cp a, $60
	jr c, .checkNextTile
	cp a, $F4
	jr z, .touchedCoin
	cp a, $77
	jr z, .touchedSidewaysPipe
	cp a, $F2				; downward fist Genkotsu
	jr z, Jmp_1B45			; ...makes Mario win? Bug? Deleted content?
.stopMario
	ld hl, $C20B			; animation frame counter
	inc [hl]
	ld a, 2
	ld [$C20E], a			; 02 walking, 04 runnning
	ld a, $FF
	ret

.checkNextTile
	ld d, -4				; if Super Mario, check collision again 4 units higher
	dec e
	jr nz, .checkSide

.noCollision
	xor a
	ret

.touchedCoin
	push hl
	pop de
	ld hl, $FFEE
	ld a, [hl]
	and a
	ret nz					; return if collision already being handled
	ld [hl], $C0			; C0in
	inc l
	ld [hl], d
	inc l
	ld [hl], e				; store block address in FFEF-FFF0
	ld a, SOUND_COIN
	ld [wSquareSFX], a			; coin sound effect
	xor a
	ret

.touchedSidewaysPipe
	ldh a, [hIsUnderground]
	and a
	jr z, .stopMario		; do nothing if we're not underground
	ld a, $0B
	ldh [hGameState], a
	ld a, $80
	ld [wC204], a		; mario in control?
	ld hl, wMarioPosX		; X pos
	ldd a, [hl]
	add a, $18
	ldh [$FFF8], a		; goal X?
	ld a, [hl]			; Y pos
	and a, $F8			; zero lowest three bits
	add a, 6
	ld [hl], a
	call ClearSprites		; clears sprites of some sort
	ld a, $FF
	ret

; makes Mario win?
Jmp_1B45:: ; 1B45
	ldh a, [hSuperStatus]
	cp a, 2						; fully grown Super Mario
	ld b, $FF
	jr z, .jmp_1B52
	ld b, $0F
	xor a
	ldh [hSuperStatus], a
.jmp_1B52
	ld a, [wAnimIndex]			; animation index
	and b
	ld [wAnimIndex], a
	ld b, a
	and a, $0F
	cp a, $0A
	jr nc, .jmp_1B66			; jmp if animation index >= 0A, which is airplane, sub stuff
	ld a, b
	and a, $F0
	ld [wAnimIndex], a
.jmp_1B66
	ld a, STATE_7					; end of level with music?
	ldh [hGameState], a
	ld a, [$D007]
	and a
	jr nz, .jmp_1B79
	ld a, $01
	ld [wActiveMusic], a		; start victory music
	ld a, $F0
	ldh [hTimer], a				; countdown
.jmp_1B79
	call ClearSprites
	xor a
	ld [wMarioVisible], a		; make mario visible again?
	ld [wGameTimerExpiringFlag], a
	ldh [rTMA], a
	ret

; collision with blocks and coins
; called every frame
BlockCollision:: ; 1B86
	xor a
	ld [$C0E2], a
	ldh a, [$FFFE]
	and a
	call nz, AddCoin
	ld hl, $FFEE		; source of the coin? :/
	ld a, [hl]
	cp a, 1			; breakable block which will break
	jr z, .jmp_1BBA
	cp a, 2			; breakable/coin block which will bounce
	jp z, .jmp_1BF7		; why is this a JP? Bug?
	cp a, $C0			; floating coin?
	jr z, .jmp_1BBA
	cp a, 4			; bounced block which will cease to be a sprite
	ret nz
	ld [hl], $00
	inc l				; hl ← FFEF
	ld d, [hl]
	inc l				; hl ← FFF0
	ld e, [hl]			; de ← place in VRAM where the coin was?
	ld a, [wOAMBuffer + $2E]		; coin block OAM sprite 3rd byte?
	cp a, $82				; normal breakable block?
	jr z, .placeBlockBackInBG
	cp a, BLOCK_COIN			; coin block
	call z, AddCoin
	ld a, BLOCK_USED			; dark block (used coinblock)
.placeBlockBackInBG
	ld [de], a
	ret

.jmp_1BBA
	ld b, [hl]
	ld [hl], 0
.jmp_1BBD				; replace coin and breakable block?
	inc l				; hl ← FFEF
	ld d, [hl]
	inc l
	ld e, [hl]			; same as above
	ld a, " "			; empty tile
	ld [de], a			; remove block
	ld a, b
	cp a, $C0
	jr z, .coinHit		; coin hit?
	ld hl, -32			; check one tile higher for a coin
	add hl, de
	ld a, [hl]
	cp a, $F4			; coin "$"
	ret nz
	ld [hl], " "		; remove it
	ld a, SOUND_COIN
	ld [wSquareSFX], a		; todo sound effect
	ld a, h
	ld [$FFB0], a
	ld a, l
	ldh [$FFAF], a
	call Call_3F13
	ldh a, [hScrollX]
	ld b, a
	ldh a, [$FFAE]
	sub b
	ldh [hFloatyX], a	; todo comment
	ldh a, [$FFAD]
	add a, $14
	ldh [hFloatyY], a
	ld a, $C0
	ldh [hFloatyControl], a
	call AddCoin
	ret

.jmp_1BF7
	ld [hl], 3
	jr .jmp_1BBD

.coinHit
	call AddCoin
	ret

; add one coin. Earns a life if 100 are collected
AddCoin:: ; 1BFF
	ldh a, [$FF9F]
	and a
	ret nz
	push de
	push hl
	ld de, $0100
	call AddScore
	pop hl
	pop de
	ldh a, [hCoins]
	add a, 1
	daa
	ldh [hCoins], a
	and a
	jr nz, DisplayCoins
	inc a
	ld [wLivesEarnedLost], a		; award a life for collecting 100 coins

DisplayCoins::; 1C1B
	ldh a, [hCoins]
	ld b, a
	and a, $0F
	ld [vBGMap0 + SCRN_VX_B + 10], a		; coins ones
	ld a, b
	and a, $F0
	swap a
	ld [vBGMap0 + SCRN_VX_B + 9], a		; coin tens
	xor a
	ldh [$FFFE], a
	inc a
	ld [$C0E2], a
	ret

; 1C33
UpdateLives::
	ldh a, [$FF9F]	; Demo mode?
	and a
	ret nz
	ld a, [wLivesEarnedLost]		; FF removes one life, 
	or a							; any other non-zero value adds one
	ret z
	cp a, $FF			; FF = -1
	ld a, [wLives]
	jr z, .loseLife
	cp a, $99			; Saturate at 99 lives
	jr z, .out
	push af
	ld a, $08
	ld [wSquareSFX], a
	ldh [$FFD3], a
	pop af
	add a, 1			; Add one life
.displayUpdatedLives
	daa
	ld [wLives], a

.displayLives
	ld a, [wLives]
	ld b, a
	and a, $0F
	ld [vBGMap0 + 7], a
	ld a, b
	and a, $F0
	swap a
	ld [vBGMap0 + 6], a		; TODO Gives these fellas a name
.out
	xor a
	ld [wLivesEarnedLost], a
	ret
.gameOver
	ld a, STATE_PRE_GAMEOVER	; Game over
	ldh [hGameState], a
	ld [$C0A4], a
	jr .out

.loseLife
	and a
	jr z, .gameOver		; No lives anymore
	sub a, 1			; Subtract one life
	jr .displayUpdatedLives

PrepareGameOver::	; 1C7C
	ld hl, vBGMap1
	ld de, GameOverText
	ld b, GameOverTextEnd - GameOverText
.loop
	ld a, [de]
	ld c, a
	WAIT_FOR_HBLANK
	WAIT_FOR_HBLANK
	ld [hl], c
	inc l
	inc de
	dec b
	jr nz, .loop
	ld a, $10
	ld [wActiveMusic], a
	ldh a, [hWorldAndLevel]			; level on which we were? BCD encoded
	ld [wContinueWorldAndLevel], a
	ld a, [wScore + 2]
	and a, $F0				; hundred thousands
	swap a
	ld b, a
	ld a, [wNumContinues]
	add b					; add a continue for every 100k points
	cp a, 10
	jr c, .nineOrLess
	ld a, 9					; saturates at nine continues
.nineOrLess
	ld [wNumContinues], a
	ld hl, wOAMBuffer
	xor a
	ld b, 160
.clearOAM
	ldi [hl], a
	dec b
	jr nz, .clearOAM

	ld [wGameTimerExpiringFlag], a
	ldh [rTMA], a
	ld hl, rWY
	ld [hl], 143
	inc hl					; rWX
	ld [hl], 7				; left edge of the screen
	ld a, $FF
	ldh [$FFFB], a			; probably timer of some sort
	ld hl, hGameState
	inc [hl]				; STATE_PRE_GAMEOVER → STATE_GAMEOVER
	ret

GameOverText::				
	db	"     game  over  "
GameOverTextEnd:

; game over animation and wait until menu
HandleGameOver:: ; 1CE8
	ld a, [wGameOverTimerExpired]
	and a
	call nz, HandleTheEnd.resetToMenu
	ret

; prepare time up
PrepareTimesUp:: ; 1CF0
	ld hl, vBGMap1			; tile map for window
	ld de, TimeUpText
	ld c, TimeUpTextEnd - TimeUpText
.loop
	ld a, [de]
	ld b, a
	WAIT_FOR_HBLANK
	ld [hl], b
	inc l
	inc de
	dec c
	jr nz, .loop	; copy the words "time up" into vram
	ld hl, rLCDC
	set 5, [hl]		; turn on window
	ld a, $A0
	ldh [hTimer], a
	ld hl, hGameState
	inc [hl]		; STATE_PRE_TIMES_UP → STATE_TIMES_UP
	ret

TimeUpText::
	db " time up "
TimeUpTextEnd:

; time up. Run out frame timer
HandleTimesUp:: ; 1D1D
	ldh a, [hTimer]
	and a
	ret nz
	ld a, STATE_DEAD	; dead
	ldh [hGameState], a
	ret

; called every frame
MoveMario::
	ld hl, wMarioWalkingDir		; mario going left (20) or right (10), changing dir (01)??
	ld a, [hl]
	cp a, 1				; changing direction?
	jr nz, .notReversing
	dec l				; C20C, momentum?
	ld a, [hl]
	and a
	jr nz, .jmp_1D38
	inc l
	ld [hl], $00		; C20D again
	jr .jmp_1D71

.jmp_1D38
	dec [hl]			; decrease C20C, momentum?
	ret
.notReversing
	ld hl, wMarioMomentum		; momentum?
	ldi a, [hl]
	cp a, $06			; max momentum
	jr nz, .jmp_1D49
	inc l				; C20E. 02 when walking, 04 when running
	ld a, [hl]
	and a
	jr nz, .jmp_1D49
	ld [hl], $02		; at max momentum, go from 0 to 2?
.jmp_1D49
	ld de, wJumpStatus		; jump status. 00 on ground, 01 ascending, 02 descending
	ldh a, [hJoyHeld]
	bit D_DOWN_BIT, a			; down button
	jr nz, .downButton
.checkDirectionalKeys
	bit D_RIGHT_BIT, a			; right button
	jr nz, .rightButton
	bit D_LEFT_BIT, a			; left button
	jp nz, .leftButton
	ld hl, wMarioMomentum		; speed?
	ld a, [hl]
	and a
	jr z, .jmp_1D6B
	xor a				; from here on we have a non-zero speed, and the movement buttons are not pressed. I think
	ld [$C20E], a		; 02 walking 04 running
	dec [hl]
	inc l				; C20B
	ld a, [hl]
	jr .checkDirectionalKeys		; keep checking for left-right keys until C20C is 0
.jmp_1D6B
	inc l				; C20D
	ld [hl], 0
	ld a, [de]
	and a
	ret nz
.jmp_1D71
	ld a, [wJumpStatus]		; jump status
	and a
	ret nz				; no animation in the air
	ld hl, wAnimIndex		; animation index
	ld a, [hl]
	and a, $F0			; upper nibble is super status
	ld [hl], a			; stand still animation
	ld a, 1
	ld [$C20B], a		; animation frame counter
	xor a
	ld [$C20E], a		; walking nor runnning
	ret
.downButton
	push af
	ldh a, [hSuperStatus]
	cp a, 2				; full grown super mario
	jr nz, .skipCrouch	; small mario can't crouch
	ld a, [de]			; de is C207
	and a
	jr nz, .skipCrouch	; cannot crouch when jumping
	ld a, $18
	ld [wAnimIndex], a		; crouching mario, hidden daisy
	ldh a, [hJoyHeld]
	and a, D_LEFT | D_RIGHT	; test bits left and right button
	jr nz, .jmp_1DA6
	ld a, [wMarioMomentum]		; momentum?
	and a
	jr z, .jmp_1DA6
.skipCrouch
	pop af
	jr .checkDirectionalKeys	; neither left nor right are being held
.jmp_1DA6
	xor a				; no stopping animation going into crouch?
	ld [wMarioMomentum], a		; speed?
	pop af
	ret

.rightButton
	ld hl, wMarioWalkingDir		; walking dir
	ld a, [hl]
	cp a, $20
	jr nz, .skip
	jp .reverseDirection	; too far to do a JR

.skip
	ld hl, wMarioFacingDir		; dir facing
	ld [hl], $00		; facing right
	call Call_1AAD
	and a
	ret nz
.jmp_1DC1
	ldh a, [hJoyHeld]
	bit 4, a			; right
	jr z, .jmp_1DE4
	ld a, [wAnimIndex]		; animation index
	cp a, $18			; crouching
	jr nz, .jmp_1DD8
	ld a, [wAnimIndex]		; why
	and a, $F0
	or a, $01
	ld [wAnimIndex], a		; first animation?
.jmp_1DD8
	ld hl, wMarioMomentum		; ...
	ld a, [hl]
	cp a, $06
	jr z, .jmp_1DE4
	inc [hl]			; increase momentum
	inc l				; wMarioWalkingDir left right changing dir
	ld [hl], $10		; going right
.jmp_1DE4
	ld hl, wMarioPosX		; on screen X pos
	ldh a, [hIsUnderground]
	and a
	jr nz, .jmp_1E21
	ld a, [$C0D2]
	cp a, $07
	jr c, .jmp_1DF9
	ldh a, [hScrollX]
	and a, $0C			; %1100
	jr z, .jmp_1E21
.jmp_1DF9
	ld a, $50			; ~ middle of screen
	cp [hl]				; C202?
	jr nc, .jmp_1E21	; JR if mario screen X <= 50
	call DetermineWalkingSpeed		; determine walking speed
	ld b, a
	ld hl, hScrollX
	add [hl]			; scroll screen if Mario is in the middle
	ld [hl], a
	call .call_1EA4		; shift sprites
	call ScrollEnemiesByB
	ld hl, $C001		; projectile X positions
	ld de, $0004		; 4 bytes per object
	ld c, 3				; 3 projectiles
.shiftProjectiles		; todo name
	ld a, [hl]
	sub b
	ld [hl], a
	add hl, de
	dec c
	jr nz, .shiftProjectiles
.jmp_1E1C
	ld hl, $C20B		; frames a dir is held?
	inc [hl]
	ret

.jmp_1E21				; right button held, mario in middle of screen
	call DetermineWalkingSpeed		; determine walking speed
	add [hl]
	ld [hl], a
	ldh a, [hGameState]
	cp a, STATE_AUTOSCROLL_LEVEL			; autoscroll
	jr z, .jmp_1E1C
	ld a, [$C0D2]
	and a
	jr z, .jmp_1E1C
	ldh a, [hScrollX]
	and a, ~%11
	ldh [hScrollX], a
	ld a, [hl]
	cp a, $A0
	jr c, .jmp_1E1C
	jp Jmp_1B45			; huh?

.leftButton
	ld hl, wMarioWalkingDir		; walking dir
	ld a, [hl]
	cp a, $10			; walking right
	jr nz, .jmp_1E61
.reverseDirection
	ld [hl], 1		; reverse dir?
	dec l
	ld [hl], 8		; C20C speed? momentum?
	ld a, [wJumpStatus]		; jump status
	and a
	ret nz					; nz if in air
	ld hl, wAnimIndex		; animation
	ld a, [hl]
	and a, $F0
	or a, 5				; reversing animation
	ld [hl], a
	ld a, $01
	ld [$C20B], a		; restart animation counter
	ret
.jmp_1E61
	ld hl, wMarioFacingDir		; dir facing
	ld [hl], $20		; facing left
	call Call_1AAD
	and a
	ret nz
	ld hl, wMarioPosX		; mario x pos
	ld a, [hl]
	cp a, $0F
	jr c, .jmp_1E9F		; jump if X < 0x0F
	push hl
	ldh a, [hJoyHeld]
	bit D_LEFT_BIT, a		; left button
	jr z, .jmp_1E97
	ld a, [wAnimIndex]
	cp a, $18			; crouching
	jr nz, .jmp_1E8B
	ld a, [wAnimIndex]
	and a, $F0
	or a, 1
	ld [wAnimIndex], a
.jmp_1E8B
	ld hl, wMarioMomentum		; speed?
	ld a, [hl]
	cp a, $06
	jr z, .jmp_1E97
	inc [hl]
	inc l				; C20D
	ld [hl], $20		; walking left
.jmp_1E97
	pop hl				; hl = C202, mario's x pos
	call DetermineWalkingSpeed
	cpl
	inc a
	add [hl]
	ld [hl], a
.jmp_1E9F
	ld hl, $C20B
	dec [hl]
	ret

; subtract B from X coord of objects 0x0C to 0x14?
.call_1EA4
	ld hl, wOAMBuffer + $C * 4 + 1
	ld de, $0004
	ld c, 8
.loop
	ld a, [hl]
	sub b
	ld [hl], a
	add hl, de
	dec c
	jr nz, .loop
	ret

DetermineWalkingSpeed
	push de
	push hl
	ld hl, SpeedData
	ld a, [$C20E]		; 02 walking 04 running
	ld e, a
	ld d, 0
	ld a, [$C20F]		; 01 standing still, flips between 1 and 0 walking
	xor a, 1
	ld [$C20F], a
	add e
	ld e, a
	add hl, de
	ld a, [hl]
	pop hl
	pop de
	ret

SpeedData::
	;  0  x  2  x  4  x
	db 0, 1, 1, 1, 1, 2

; Clears objects 0-3, 7 to 20. Projectiles, fragments of blocks, score
ClearSprites:: ; 1ED4
	push hl
	push bc
	push de
	ld hl, wOAMBuffer + 7 * 4
	ld b, 13 * 4
	xor a
.clearLoop
	ldi [hl], a
	dec b
	jr nz, .clearLoop
	ld hl, wOAMBuffer
	ld b, 11		; 2 objects and 3/4th of one ? Bug?
.clearLoop2
	ldi [hl], a
	dec b
	jr nz, .clearLoop2
	ldh [$FFA9], a		; projectile status
	ldh [$FFAA], a
	ldh [$FFAB], a
	ld hl, $C210		; todo with fragments of blocks
	ld de, $0010
	ld b, $04
	ld a, $80
.clearLoop3
	ld [hl], a
	add hl, de
	dec b
	jr nz, .clearLoop3
	pop de
	pop bc
	pop hl
	ret

Call_1F03:: ; 1F03
	ldh a, [hFrameCounter]
	and a, 3
	ret nz				; every 4 frames
	ld a, [wInvincibilityTimer]
	and a
	ret z
	cp a, 1
	jr z, .endOfInvincibility
	dec a
	ld [wInvincibilityTimer], a
	ld a, [wMarioVisible]
	xor a, SPRITE_HIDDEN ; blink Mario 7.5 times per second
	ld [wMarioVisible], a
	ld a, [$DFE9]		; currently playing song
	and a
	ret nz				; invincibility stops when the timer runs out,
.endOfInvincibility		; or the song stops
	xor a
	ld [wInvincibilityTimer], a
	ld [wMarioVisible], a		; mario visible
	call StartLevelMusic
	ret

; called every frame in non autoscroll levels
Call_1F2D:: ; 1F2D
	ld b, 1				; just one superball?
	ld hl, $FFA9		; projectiles at A9, AA and AB?
	ld de, wOAMBuffer + 1 ; objects 0, X position
.superballLoop
	ldi a, [hl]
	and a
	jr nz, .moveSuperball
.nextSuperball			; XXX more than one? how?
	inc e
	inc e
	inc e
	inc e				; next object
	dec b
	jr nz, .superballLoop
	ret

.moveSuperball
	push hl
	push de
	push bc
	dec l				; hl = FFA9
	ld a, [wSuperballTTL]
	and a
	jr z, .removeSuperball
	dec a
	ld [wSuperballTTL], a
	bit 0, [hl]			; going right?
	jr z, .flyingLeft
	ld a, [de]			; X pos
	inc a
	inc a
	ld [de], a			; X → X + 2
	cp a, $A2
	jr c, .detectCollisionRight
.removeSuperball
	xor a
	res 0, e			; Guess they remembered this CPU has bit instructions
	ld [de], a			; a DEC DE would have sufficed
	ld [hl], a
	jr .enemyCollision

.detectCollisionRight
	add a, 3			; check collision a little in front
	push af
	dec e				; e is now the Y coord of the object
	ld a, [de]
	ldh [$FFAD], a		; used in collision detection
	pop af
	call FindNeighboringTile
	jr c, .verticalMotion	; c if no collision with solid
	ld a, [hl]
	and a, %11111100
	or a,  %00000010	; reverse direction, go left
	ld [hl], a
.verticalMotion
	bit 2, [hl]			; non zero if going up
	jr z, .flyingDown
	ld a, [de]
	dec a
	dec a
	ld [de], a
	cp a, $10
	jr c, .removeSuperball		; c if out of bounds
	sub a, 1
	ldh [$FFAD], a				; check collision uo
	inc e
	ld a, [de]
	call FindNeighboringTile
	jr c, .enemyCollision		; c if no collision
	ld a, [hl]
	and a, %11110011
	or  a, %00001000	; reverse direction, go down
	ld [hl], a
.enemyCollision
	pop bc
	pop de
	pop hl
	call Call_200A		; collision with enemy?
	jr .nextSuperball

.flyingDown
	ld a, [de]
	inc a
	inc a
	ld [de], a
	cp a, $A8				; todo screen width and such
	jr nc, .removeSuperball	; nc if out of bounds
	add a, 4				; check collision down
	ldh [$FFAD], a
	inc e
	ld a, [de]
	call FindNeighboringTile
	jr c, .enemyCollision		; c if no collision
	ld a, [hl]
	and a, %11110011
	or  a, %00000100	; reverse direction, go up
	ld [hl], a
	jr .enemyCollision

.flyingLeft
	ld a, [de]
	dec a
	dec a
	ld [de], a
	cp a, 4
	jr c, .removeSuperball		; if out of bounds
	sub a, 2			; detect collision to the left
	push af
	dec e
	ld a, [de]
	ldh [$FFAD], a
	pop af
	call FindNeighboringTile
	jr c, .verticalMotion
	ld a, [hl]
	and a, %11111100
	or a,  %00000001	; reverse direction, go right
	ld [hl], a
	jr .verticalMotion

; FFAD is preloaded with the Y coord, A contains X coord
FindNeighboringTile::	; gets called in autoscroll from 514F?
	ld b, a
	ldh a, [hScrollX]
	add b
	ldh [$FFAE], a
	push de
	push hl
	call LookupTile
	cp a, $F4			; Coin sprite
	jr nz, .checkForBreakableBlock
	ldh a, [hGameState]
	cp a, $0D			; In autoscroll levels, can't collect coins with projectiles
	jr z, .checkForSolidTile
	push hl
	pop de
	ld hl, $FFEE		; collision?
	ld a, [hl]
	and a
	jr nz, .checkForSolidTile
	ld [hl], $C0		; COin?
	inc l
	ld [hl], d
	inc l
	ld [hl], e
	ld a, SOUND_COIN
	ld [wSquareSFX], a		; coin sound effect
.checkForBreakableBlock
	cp a, $82			; breakable block
	call z, Call_200A.breakBlockInAutoscroll
	cp a, $80			; breakable block
	call z, Call_200A.breakBlockInAutoscroll
.checkForSolidTile
	pop hl
	pop de
	cp a, $60			; every tile above $60 is solid
	ret

Call_200A::
	push hl
	push de
	push bc
	ld b, $0A
	ld hl, $D100
.enemyLoop				; todo not just enemies?
	ld a, [hl]
	cp a, $FF			; no enemy
	jr nz, .jmp_2029
.nextEnemy
	push de
	ld de, $0010
	add hl, de
	pop de
	dec b
	jr nz, .enemyLoop
	pop bc
	pop de
	pop hl
	ret

.popRegsAndNextEnemy
	pop hl
	pop de
	pop bc
	jr .nextEnemy

.jmp_2029
	push bc
	push de
	push hl
	ld bc, $000A		; lower 7 bits is width + height
	add hl, bc			; platform = B1? some sort of "type" at least
	bit 7, [hl]			; if bit 7 is set, they can't be hit
	jr nz, .popRegsAndNextEnemy
	ld c, [hl]
	inc l
	inc l				; D1XC = health?
	ld a, [hl]
	ldh [$FF9B], a		; more new variables
	ld a, [de]
	ldh [hBoundingBoxLeft], a		; de is still X pos of superball
	add a, 4			; X pos + 4
	ldh [hBoundingBoxRight], a
	dec e				; y pos
	ld a, [de]
	ldh [hBoundingBoxTop], a
	ld a, [de]
	add a, 3			; y pos + 3
	ldh [hBoundingBoxBottom], a		; FF8F FFA0 FFA1 FFA2, some sort of hitbox?
	pop hl
	push hl
	call BoundingBoxCollision		; hit detection?
	and a
	jr z, .popRegsAndNextEnemy
	dec l
	dec l
	dec l
	call Call_A10
	push de
	ldh a, [hGameState]
	cp a, $0D
	jr nz, .jmp_2064
	call Call_2AAD
	jr .jmp_2067

.jmp_2064
	call Call_2A68
.jmp_2067
	pop de
	and a
	jr z, .popRegsAndNextEnemy
	push af
	ld a, [de]
	sub a, 8
	ldh [hFloatyY], a		; todo comment
	inc e
	ld a, [de]
	ldh [hFloatyX], a
	pop af
	cp a, $FF
	jr nz, .jmp_2083
	ld a, SOUND_STOMP
	ld [wSquareSFX], a			; enemy dieing sound effect
	ldh a, [$FF9E]
	ldh [hFloatyControl], a
.jmp_2083
	xor a
	ld [de], a
	dec e
	ld [de], a
	ld hl, $FFAB
	bit 3, e
	jr nz, .jmp_2094
	dec l
	bit 2, e
	jr nz, .jmp_2094
	dec l
.jmp_2094
	ld [hl], a
	jr .popRegsAndNextEnemy

.breakBlockInAutoscroll
	push hl
	push bc
	push de
	push af
	ldh a, [hGameState]
	cp a, STATE_AUTOSCROLL_LEVEL
	jr nz, .out				; only breakable in autoscroll
	push hl
	pop de
	ld hl, $FFEE			; collision?
	ld a, [hl]
	and a
	jr nz, .out
	ld [hl], 1
	inc l					; FFEF
	ld [hl], d
	inc l					; FFF0
	ld [hl], e
	pop af
	push af
	cp a, $80
	jr nz, .jmp_20C1
	ld a, d
	add a, $30
	ld d, a
	ld a, [de]
	and a
	jr z, .jmp_20C1
	call Call_254D			; spawn powerup
.jmp_20C1
	ld hl, $C210			; non player entity?
	ld de, $0010
	ld b, 4
.jmp_20C9
	push hl
	ld [hl], $00
	inc l
	ldh a, [$FFAD]			; Y coordinate in VRAM?
	add a, $00
	ld [hl], a
	inc l
	ldh a, [hBoundingBoxBottom]
	add a, $00
	ld [hl], a
	inc l
	inc l
	inc l
	inc l
	inc l
	ld [hl], 1
	inc l
	ld [hl], 7
	pop hl
	add hl, de
	dec b
	jr nz, .jmp_20C9
	ld hl, $C222
	ld a, [hl]
	sub a, 4
	ld [hl], a
	ld hl, $C242
	ld a, [hl]
	sub a, 4
	ld [hl], a
	ld hl, $C238
	ld [hl], $0B
	ld hl, $C248
	ld [hl], $0B
	ldh a, [hScrollX]
	ldh [$FFF3], a
	ld de, $0050
	call AddScore
	ld a, 2
	ld [wNoiseSFX], a
.out
	pop af
	pop de
	pop bc
	pop hl
	ret

Call_2113:: ; 2113
	ldh a, [$FF9F]
	and a
	ret z
	ld a, [$C0DB]
	ldh [hJoyHeld], a
	ret

InitialStateData::
	; C200. Mario's position, state, animation etc..
	;  vis  			Y    X   spr  	  flip		    		  jmp state
	db SPRITE_VISIBLE, 134, 50, $00, $00, SPRITE_NO_FLIP, $00, MARIO_ON_GROUND, $00, $00
	ds 6
	; C210 - C240. Fragments from breakable block, clouds, Daisy, spaceship
	;       Y  X   sprite    flip           TTL
	db $01, 0, 0, $0F, $00, $00, $00, $00, $00
	ds 7
	db $01, 0, 0, $0F, $00, SPRITE_XFLIP, $00, $00, $00
	ds 7
	db $01, 0, 0, $0F, $00, SPRITE_NO_FLIP, $00, $00, $00
	ds 7
	db $01, 0, 0, $0F, $00, SPRITE_XFLIP, $00, $00, $00
	ds 7

Data_216D::	 ; jumping "parabola"?
	db $04, $04, $03, $03, $02, $02, $02, $02, $02, $02, $02, $02, $02, $01, $01, $01, $01, $01, $01, $01, $00, $01, $00, $01, $00, $00, $7F

Jump_2188:: ; 2188
	ld a, 3
	ldh [$FFEA], a		; has to do with rendering
	ldh a, [hScrollX]
	ld b, a
	ld a, [$C0AA]		; another scroll thingy, rounded to 8 pxs?
	cp b
	ret z
	xor a
	ldh [$FFEA], a
	ret

LoadColumns:: ; 2198
	ldh a, [$FFEA]
	and a
	jr nz, Jump_2188
	ldh a, [hScrollX]
	and a, 8
	ld hl, $FFA3		; switches between 0 and 8
	cp [hl]
	ret nz
	ld a, [hl]
	xor a, 8
	ld [hl], a
	and a
	jr nz, LoadNextColumn
	ld hl, $C0AB		; increments every two columns?
	inc [hl]

; decompress a column from the level
LoadNextColumn::	; 21B1
	ld b, $10			; the screen without hud is exactly 16 tiles high
	ld hl, $C0B0		; tilemap column cache todo
	ld a, " "			; blank tile
.clearLoop
	ldi [hl], a
	dec b
	jr nz, .clearLoop
	ldh a, [hColumnIndex]
	and a
	jr z, .startNewScreen		; if zero, start a new screen
	ldh a, [hColumnPointerHi]	; pointer to next column?
	ld h, a
	ldh a, [hColumnPointerLo]	; eww big endian
	ld l, a
	jr .decodeLoop

.startNewScreen
	ld hl, LevelPointers	; table of 4*3 + 1 pointers
	ldh a, [hLevelIndex]
	add a
	ld e, a
	ld d, $00
	add hl, de			; hl = $4000 + level * 2
	ld e, [hl]
	inc hl
	ld d, [hl]
	push de
	pop hl				; hl ← [$4000 + level * 2] ; address of table of screen
	ldh a, [hScreenIndex]
	add a				; * 2
	ld e, a
	ld d, $00
	add hl, de			; hl ← [hl + screen * 2]
	ldi a, [hl]
	cp a, $FF			; end of level?
	jr z, .endOfLevel
	ld e, a				; if not, this is a pointer to the screen?
	ld d, [hl]
	push de
	pop hl

.decodeLoop
	ldi a, [hl]
	cp a, $FE
	jr z, .endOfColumn	; $FE = end of column
	ld de, $C0B0
	ld b, a
	and a, $F0			; high nibble = offset into column
	swap a
	add e
	ld e, a
	ld a, b
	and a, $0F			; low nibble = number of tiles following
	jr nz, .skip
	ld a, $10			; if count is zero, fill column
.skip
	ld b, a
.nextRow
	ldi a, [hl]
	cp a, $FD			; FD means fill the rest with the next byte
	jr z, .repeatNextTile
	ld [de], a
	cp a, BLOCK_PIPE_OPENING	; vertical pipe left tile
	jr nz, .notPipe
	call CheckPipeForWarp
	jr .incrementRow

.notPipe
	cp a, BLOCK_BREAKABLE		; breakable block?
	jr nz, .notBreakableBlock
	call CheckBlockForItem
	jr .incrementRow

.notBreakableBlock
	cp a, BLOCK_HIDDEN		; hidden block?
	jr nz, .notHiddenBlock
	call CheckBlockForItem
	jr .incrementRow

.notHiddenBlock
	cp a, BLOCK_COIN		; coin block
	call z, CheckBlockForItem
.incrementRow
	inc e
	dec b
	jr nz, .nextRow
	jr .decodeLoop

.endOfLevel
	ld hl, $C0D2		; starts incrementing when the end of level is reached
	inc [hl]
	ret

.endOfColumn
	ld a, h
	ldh [hColumnPointerHi], a
	ld a, l
	ldh [hColumnPointerLo], a		; save a pointer to the next column
	ldh a, [hColumnIndex]
	inc a
	cp a, SCRN_X_B	; 20 columns per screen?
	jr nz, .screenNotFinished
	ld hl, hScreenIndex	; next block
	inc [hl]
	xor a
.screenNotFinished
	ldh [hColumnIndex], a
	ldh a, [hScrollX]
	ld [$C0AA], a
	ld a, $01
	ldh [$FFEA], a
	ret

.repeatNextTile
	ld a, [hl]
.loop
	ld [de], a
	inc e
	dec b
	jr nz, .loop
	inc hl
	jp .decodeLoop

; draw a column in the tile map
DrawColumn:: ; 2258
	ldh a, [$FFEA]		; 01 if a new column needs to be loaded, 03 if we're 
	cp a, 1				; still standing on that spot, but we don't need a new 
	ret nz				; one, 00 otherwise. or more complicated...
	ldh a, [$FFE9]		; FFE9 holds the lower byte of the address of the first
	ld l, a				; column not yet loaded. Upper byte is always 98
	inc a
	cp a, $60			; todo constants. VRAM is $60 - $40 tiles wide
	jr nz, .noWrapAround
	ld a, $40
.noWrapAround
	ldh [$FFE9], a
	ld h, HIGH(vBGMap0)
	ld de, $C0B0		; the next column is preloaded here by something
	ld b, $10			; 16 tiles high
.nextRow
	push hl
	ld a, h
	add a, $30
	ld h, a
	ld [hl], 0
	pop hl
	ld a, [de]
	ld [hl], a			; set the tile in the tilemap
	cp a, BLOCK_PIPE_OPENING	; pipe opening
	jr nz, .notPipe
	call Call_22FD
	jr .incrementRow

.notPipe
	cp a, BLOCK_BREAKABLE	; breakable block
	jr nz, .notBreakableBlock
	call StoreBlockInOverlay
	jr .incrementRow

.notBreakableBlock
	cp a, BLOCK_HIDDEN	; hidden block
	jr nz, .notHiddenBlock
	call StoreBlockInOverlay
	jr .incrementRow

.notHiddenBlock
	cp a, BLOCK_COIN	; coin block
	call z, StoreBlockInOverlay
.incrementRow
	inc e
	push de
	ld de, SCRN_VX_B	; todo screenwidth or w/e
	add hl, de
	pop de
	dec b
	jr nz, .nextRow
	ld a, 2
	ldh [$FFEA], a
	ret

; Does a lookup if this pipe is a warp pipe. Store data temporarily in HRAM
CheckPipeForWarp:: ; 22A9
	push hl
	push de
	push bc
	ldh a, [hIsUnderground]		; $0A in underground?
	and a
	jr nz, .out
	SAVE_AND_SWITCH_ROM_BANK 3
	ldh a, [hLevelIndex]
	add a
	ld e, a
	ld d, $00
	ld hl, $651C		; extract this todo
	add hl, de
	ld e, [hl]
	inc hl
	ld d, [hl]
	push de
	pop hl				; hl ← [651C + 2 * level], pointer
.checkPipeForWarp
	ldh a, [hScreenIndex]	; first byte is level screen of warping pipe
	cp [hl]
	jr z, .matchingLevelScreen
	ld a, [hl]
	cp a, $FF				; ff is end of list
	jr z, .restoreROMBankAndOut
	inc hl
.nextPipe
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	jr .checkPipeForWarp

.matchingLevelScreen
	ldh a, [hColumnIndex]	; second byte is column on which the pipe is
	inc hl
	cp [hl]
	jr nz, .nextPipe
	inc hl
	ld de, $FFF4			; room to which the pipe leads
	ldi a, [hl]
	ld [de], a
	inc e					; FFF5, room where you exit
	ldi a, [hl]
	ld [de], a
	inc e					; FFF6 x position where you leave pipe
	ldi a, [hl]
	ld [de], a
	inc e					; FFF7 y position where you leave pipe
	ld a, [hl]
	ld [de], a
.restoreROMBankAndOut
	RESTORE_ROM_BANK
.out
	pop bc
	pop de
	pop hl
	ret

; hl contains the location in VRAM of the pipe?
Call_22FD::
	ldh a, [$FFF4]	; is non-zero in underground, or when nearing a pipe?
	and a
	ret z
	push hl
	push de
	ld de, -$20
	push af
	ld a, h
	add a, $30		; C8.. is an "overlay" of VRAM, with warps/hidden blocks/...
	ld h, a			; indicated
	pop af
	ld [hl], a
	ldh a, [$FFF5]
	add hl, de
	ld [hl], a
	ldh a, [$FFF6]
	add hl, de
	ld [hl], a
	ldh a, [$FFF7]
	add hl, de
	ld [hl], a
	xor a
	ldh [$FFF4], a
	ldh [$FFF5], a
	pop de
	pop hl
	ret

CheckBlockForItem:: ; 2321
	push hl
	push de
	push bc
	SAVE_AND_SWITCH_ROM_BANK 3
	ldh a, [hLevelIndex]
	add a
	ld e, a
	ld d, $00
	ld hl, $6536		; Table of items
	add hl, de			; Three bytes per item: level screen, column in screen
	ld e, [hl]			; contents
	inc hl
	ld d, [hl]
	push de
	pop hl				; table
.checkScreen
	ldh a, [hScreenIndex]
	cp [hl]
	jr z, .matchingLevelScreen
	ld a, [hl]
	cp a, $FF			; again, end of list
	jr z, .restoreROMBankAndOut
	inc hl
.nextItem
	inc hl
	inc hl
	jr .checkScreen

.matchingLevelScreen
	ldh a, [hColumnIndex]
	inc hl
	cp [hl]
	jr nz, .nextItem
	inc hl
	ld a, [hl]
	ld [$C0CD], a		; contents of block
.restoreROMBankAndOut
	RESTORE_ROM_BANK
	pop bc
	pop de
	pop hl
	ret

; stores the content of the block in overlay?
StoreBlockInOverlay:: ; 2363
	ld a, [$C0CD]
	and a
	ret z
	push hl
	push af
	ld a, h
	add a, $30		; again to the C8.. overlay?
	ld h, a
	pop af
	ld [hl], a
	xor a
	ld [$C0CD], a
	pop hl
	ret

; too many calls to far banks
HandleAutoScrollLevel::
    ldh a, [$B2]
    and a
    ret nz

    call LoadColumns
    call Call_001_4FB2
    ld a, [$D007]
    and a
    call nz, Jmp_1B45
    call EntityCollision
    call Call_01_4FEC
    call Call_01_5118
    ldh a, [hActiveRomBank]
    ldh [hSavedRomBank], a
    ld a, 3
    ldh [hActiveRomBank], a
    ld [MBC1RomBank], a
    call $498B
    ld bc, $C218
    ld hl, Data_216D
    call Call_490D
    ld bc, $C228
    ld hl, Data_216D
    call Call_490D
    ld bc, $C238
    ld hl, Data_216D
    call Call_490D
    ld bc, $C248
    ld hl, Data_216D
    call Call_490D
    call $4AEA
    call $4B8A
    call $4BB5
    ldh a, [hSavedRomBank]
    ldh [hActiveRomBank], a
    ld [MBC1RomBank], a
    call Call_2491
    ldh a, [hActiveRomBank]
    ldh [hSavedRomBank], a
    ld a, BANK(UpdateTimerAndFloaties)
    ldh [hActiveRomBank], a
    ld [MBC1RomBank], a
    call UpdateTimerAndFloaties
    ldh a, [hSavedRomBank]
    ldh [hActiveRomBank], a
    ld [MBC1RomBank], a
    call Call_1736
    call Call_01_515E
    call Call_1F03
    ldh a, [$AC]
    and 3
    ret nz

    ld a, [wAnimIndex]
    xor 1
    ld [wAnimIndex], a
    ret

AnimateBackground:: ; 2401
	ld a, [wBackgroundAnimated]
	and a
	ret z
	ldh a, [hGameState]
	cp a, STATE_AUTOSCROLL_LEVEL		; For some reason, the background is only animated
	ret nc				; in gamestates < 0D. This is mostly normal gameplay
	ldh a, [hFrameCounter]
	and a, 7			; Animate every 8 frames
	ret nz
	ldh a, [hFrameCounter]
	bit 3, a
	jr z, .secondFrame
	ld hl, $C600		; copied to here from where?
	jr .copyTile

.secondFrame
	ld hl, Data_3FC4 ; todo
	ldh a, [hWorldAndLevel]
	and a, $F0
	sub a, $10			; A is 8 * (world - 1)
	rrca
	ld d, 0
	ld e, a
	add hl, de
.copyTile
	ld de, $95D1		; only this tile is animated
	ld b, 8
.loop
	ldi a, [hl]
	ld [de], a
	inc de
	inc de				; 1BPP encoded?
	dec b
	jr nz, .loop
	ret

; Levels with or without animated background tile. Pointless
LevelsWithAnimatedBackground::
	db 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0

; wBackgroundAnimated is clear. But what are the other variables?
SetIsBackgroundAnimated::
	ld a, $0C
	ld [$C0AB], a
	call InitEnemySlots
	xor a
	ld [$D007], a
	ld hl, LevelsWithAnimatedBackground
	ldh a, [hLevelIndex]
	ld d, 0
	ld e, a
	add hl, de
	ld a, [hl]
	ld [wBackgroundAnimated], a
	ret

; initializes objects? skips over enemies?
InitEnemySlots:: ; 245C
	ld hl, LevelEnemyPointers
	ldh a, [hLevelIndex]
	rlca				; why not ADD A?... Carry will be zero anyway
	ld d, $00
	ld e, a
	add hl, de
	ldi a, [hl]
	ld e, a
	ld a, [hl]			; load address from the table
	ld d, a
	ld h, d
	ld l, e
	ld a, [$C0AB]		; "progress" in level
	ld b, a
.next
	ld a, [hl]			; enemy location?
	cp b
	jr nc, .storeFirstEnemyLocation
	inc hl
	inc hl
	inc hl
	jr .next

.storeFirstEnemyLocation
	ld a, l
	ld [$D010], a
	ld a, h
	ld [$D011], a
	ld hl, $D100		; enemies, powerups, platforms
	ld de, $0010
.loop
	ld [hl], $FF		; disable
	add hl, de
	ld a, l
	cp a, $A0			; clear D100 to D190
	jp nz, .loop		; why JP and nor JR?
	ret

Call_2491:: ; 2491
	call SpawnEnemies
	call Call_2648
	call DrawEnemies
	ret

SpawnEnemies:: ; 249B
	ld a, [$D010]
	ld l, a
	ld a, [$D011]
	ld h, a
	ld a, [hl]		; where next enemy should appear?
	ld b, a
	ld a, [$C0AB]	; progress in columns / 2
	sub b
	ret z
	ret c			; return if we're not there yet
	ld c, a
	swap c			; remember the difference in the hi nibble
	push hl
	inc hl			; Y position of the enemy, in tiles (lowest 5 bits)
	ld a, [hl]
	and a, $1F
	rlca
	rlca
	rlca			; multiply by 8
	add a, $10		; account for HUD
	ldh [$FFC2], a	; enemy buffer?
	ldi a, [hl]		; again Y position
	and a, $C0		; highest 2 bits, X position
	swap a
	add a, $D0		; TODO what's going on here
	sub c
	ldh [$FFC3], a	; future X pos
	call Call_24EF
	pop hl
	ld de, $0003	; 3 bytes per enemy
	add hl, de		; next one to spawn
	ld a, l
	ld [$D010], a
	ld a, h
	ld [$D011], a
	jr SpawnEnemies

; enemy launches projectile?
Call_24D6:: ; 24D6
	ld a, [wCommandArgument]
	ldh [$FFC0], a
	cp a, $FF
	ret z
	ld d, $00
	ld e, a
	rlca
	add e			; multiply by 3
	rl d			; propagate carry into upper nibble
	ld e, a
	ld hl, Data_3375
	add hl, de
	ldi a, [hl]
	ldh [$FFC7], a	; flags
	jr Jmp_250B

Call_24EF:: ; 24EF
	ldh a, [hWinCount]
	and a
	jr nz, .jmp_24F7
	bit 7, [hl]		; 7th bit set means the enemy appears only in expert mode
	ret nz
.jmp_24F7
	ld a, [hl]
	and a, $7F
	ldh [$FFC0],a
	ld d, $00
	ld e, a
	rlca
	add e
	rl d
	ld e, a
	ld hl, Data_3375
	add hl, de
	ld a, [hl]
	ldh [$FFC7], a	; flags

Jmp_250B:
	xor a
	ldh [$FFC4], a
	ldh [hMarioState], a
	ldh [$FFC8], a
	ldh [$FFC9], a
	ldh [$FFCB], a
	ldh a, [$FFC0]
	ld d, $00
	ld e, a
	rlca
	add e
	rl d
	ld e, a
	ld hl, Data_3375
	add hl, de
	inc hl				; not interested in the first byte?
	ldi a, [hl]
	ldh [$FFCA], a		; mortality and dimensions
	ld a, [hl]
	ldh [$FFCC], a		; "health" above C0 means boss
	cp a, $C0
	jr c, .findEmptySlot
	ld a, $0B
	ld [wActiveMusic], a		; boss music!
.findEmptySlot
	ld de, $0010
	ld b, $00
	ld hl, $D100
.slotLoop
	ld a, [hl]
	inc a
	jr z, .jmp_2548
	inc b
	add hl, de
	ld a, l
	cp a, $90			; 10 slots, from 0-9
	jr nz, .slotLoop
	ret

.jmp_2548
	ld a, b
	call CopyBufferToEnemySlot
	ret

Call_254D:: ; 254D
	ld hl, $D190		; powerup slot
	ld [hl], a
	ldh a, [$FFC2]		; Y pos
	and a, $F8
	add a, $7			; align the Y pos with block coordinates?
	ld [$D192], a
	ldh a, [$FFC3]		; X pos
	ld [$D193], a
	call InitEnemy
	ld a, $0B
	ld [wSquareSFX], a
	ret

DrawEnemies::; 2568
	xor a
	ld [wObjectsDrawn], a
	ld c, $00			; C points to the slot currently being drawn
.drawEnemySlot
	ld a, [wObjectsDrawn]
	cp a, 20			; the upper 20 objects are reserved for enemies
	ret nc
	push bc
	ld a, c
	swap a
	ld hl, $D100
	ld l, a
	ld a, [hl]
	inc a
	jr z, .nextSlot		; zero if D1x0 is FF, empty slot
	ld a, c
	call CopyEnemySlotToBuffer
	ldh a, [$FFC3]
	cp a, $E0			; the screen is A0 pixels wide. Due to wraparound,
	jr c, .checkYBound	; 00 → FF, this also takes care of enemies leaving
.enemyOutOfBounds		; stage left
	ld a, SCRN_VX - 1	; remove enemy
	ldh [$FFC0], a
	ld a, c
	call CopyBufferToEnemySlot
	jr .nextSlot

.checkYBound
	ldh a, [$FFC2]
	cp a, $C0
	jr nc, .enemyOutOfBounds
	call .drawEnemy
.nextSlot
	pop bc
	inc c
	ld a, c
	cp a, $0A
	jr nz, .drawEnemySlot
	ld hl, wOAMBuffer + 4 * $14
	ld a, [wObjectsDrawn]
	rlca
	rlca
	ld d, $00				; 4 bytes per object
	ld e, a
	add hl, de
.loop						; Offscreen remaining objects
	ld a, l
	cp a, $A0				; maximum 10 enemy slots
	jp nc, .retNC			; why not a RET NC? Bug
	ld a, 180				; 180 px is offscreen. Just using 0 would suffice
	ld [hl], a
	inc hl
	inc hl
	inc hl
	inc hl
	jr .loop

.retNC
	ret

.drawEnemy
	xor a
	ld [$D000], a			; D000 is used to temporarily store object flags
	ld hl, wOAMBuffer + 4 * $14
	ld a, [wObjectsDrawn]
	rlca
	rlca					; 4 bytes per object
	ld d, $00
	ld e, a
	add hl, de
	ld b, h
	ld c, l					; store address in BC
	ld hl, Data_2FE2
	ldh a, [hMarioState]
	and a, %1				; BIT 0, A >_> bug. 1 if facing right?
	jr nz, .jmp_25DE
	ld hl, Data_30B4
.jmp_25DE
	ldh a, [$FFC6]			; animation index/sprite index
	rlca					; times two, the table stores pointers
	ld d, $00
	ld e, a
	add hl, de
	ldi a, [hl]				; look the address up in the table
	ld e, a
	ld a, [hl]
	ld d, a					; store in DE
	ld h, d					; and from DE to HL
	ld l, e
.drawSpriteLoop
	ld a, [wObjectsDrawn]
	cp a, $14
	ret nc
.readNextByte
	ld a, [hl]
	cp a, $FF
	ret z
	bit 7, a				; Highest bit set means this is an object index
	jr nz, .drawObject		; to be drawn
	rlca					; Rotate left and reset carry (as bit 7 is not set)
	res 4, a				; Palette?
	ld [$D000], a			; Later used as object attributes. Lower 3 bits
	ld a, [hl]				; are unused there. So the lower 4 bits of
	bit 3, a				; bytes that aren't object indices, are used here
	jr z, .testBit2
	ldh a, [$FFC2]
	sub a, $8				; bit 3 - move one tile up
	ldh [$FFC2], a
	ld a, [hl]
.testBit2
	bit 2, a
	jr z, .testBit1
	ldh a, [$FFC2]
	add a, $8				; bit 2 - move one tile down
	ldh [$FFC2], a
	ld a, [hl]
.testBit1
	bit 1, a
	jr z, .testBit0
	ldh a, [$FFC3]
	sub a, $8				; bit 1 - move one tile left
	ldh [$FFC3], a
	ld a, [hl]
.testBit0
	bit 0, a
	jr z, .nextByte
	ldh a, [$FFC3]
	add a, $8				; bit 0 - move one tile right
	ldh [$FFC3], a
.nextByte
	inc hl
	jr .readNextByte

; BC contains the object address at this point
.drawObject
	ldh a, [$FFC2]		; Y pos
	ld [bc], a			; object Y pos
	inc bc
	ldh a, [$FFC3]		; X pos
	ld [bc], a			; object X pos
	inc bc
	ld a, [hl]			; this comes from the pointer 2FE2 and 30B4
	ld [bc], a
	inc bc
	ld a, [$D000]		; where is this filled in?
	ld [bc], a			; object attributes
	inc bc
	inc hl
	ld a, [wObjectsDrawn]
	inc a
	ld [wObjectsDrawn], a
	jr .drawSpriteLoop

Call_2648:: ; 2648
	ld hl, $D100
.loop
	ld a, [hl]
	inc a
	jr z, .nextSlot
	push hl
	call CopyEnemySlotToBuffer.fromHL
	ld hl, Data_349E
	ldh a, [$FFC0]		; enemy ID
	rlca
	ld d, $00
	ld e, a
	add hl, de
	ldi a, [hl]
	ld e, a
	ld a, [hl]
	ld d, a
	ld h, d
	ld l, e
	call .updateEnemy
	pop hl
	push hl
	call CopyBufferToEnemySlot.toHL
	pop hl
.nextSlot
	ld a, l
	add a, $10
	ld l, a
	cp a, $A0
	jp nz, .loop
	ret

.updateEnemy
	ldh a, [$FFC8]			;
	and a
	jr z, .runScript
	ldh a, [$FFC7]			; bit 1 set if gravity works on it?
	bit 1, a
	jr z, .jmp_2692
	call CheckCollisionWithLowerTile			; check collision one tile down?
	jr nc, .jmp_268C		; no carry means the tile is solid
	ldh a, [$FFC2]			; Y pos
	inc a					; fall down
	ldh [$FFC2], a
	ret						; resume script when back on the ground

.jmp_268C
	ldh a, [$FFC2]
	and a, $F8				; snap to tile grid
	ldh [$FFC2], a
.jmp_2692
	ldh a, [$FFC9]
	and a, $F0
	swap a
	ld b, a
	ldh a, [$FFC9]
	and a, $0F
	cp b
	jr z, .jmp_26A7
	inc b
	swap b
	or b
	ldh [$FFC9], a
	ret

.jmp_26A7
	ldh a, [$FFC9]
	and a, $0F
	ldh [$FFC9], a
	ldh a, [$FFC8]
	dec a
	ldh [$FFC8], a
	jp .moveEnemy

.runScript
	push hl
	ld d, $00
	ldh a, [$FFC4]			; script index
	ld e, a
	add hl, de
	ld a, [hl]
	ld [wCurrentCommand], a
	cp a, $FF				; end of script sentinel
	jr nz, .runCommand
	xor a					; end of script reached, restart
	ldh [$FFC4], a
	pop hl
	jr .runScript

.runCommand
	ldh a, [$FFC4]
	inc a
	ldh [$FFC4], a
	ld a, [wCurrentCommand]
	and a, $F0
	cp a, $F0
	jr z, .specialCommand	; Fx command with argument
	ld a, [wCurrentCommand]
	and a, $E0
	cp a, $E0				; Ex wait
	jr nz, .speedCommand
	ld a, [wCurrentCommand]
	and a, $0F				; wait command, wait the lower nibble number
	ldh [$FFC8], a			; of ticks? todo
	pop hl
	jr .updateEnemy

.speedCommand
	ld a, [wCurrentCommand]
	ldh [$FFC1], a			; the speed is just the command itself
	ld a, $01
	ldh [$FFC8], a
	pop hl
	jp .updateEnemy

.specialCommand
	ldh a, [$FFC4]			; increment script index, load argument
	inc a
	ldh [$FFC4], a
	inc hl
	ld a, [hl]
	ld [wCommandArgument], a
	ld a, [wCurrentCommand]
	cp a, $F8					; F8 - Change sprite
	jr nz, .checkF0
	ld a, [wCommandArgument]
	ldh [$FFC6], a
	pop hl
	jr .runScript

.checkF0
	cp a, $F0					; F0 - Change orientation
	jr nz, .checkF1
	ld a, [wCommandArgument]
	and a, $C0					; 1100 0000
	jr z, .checkBits2And3
	bit 7, a			; bit 7, direct towards Mario in the Y direction?
	jr z, .checkBit6
	ldh a, [hMarioState]
	and a, $FD			; unset bit 1
	ld b, a
	ld a, [wMarioPosY]		; Y pos
	ld c, a
	ldh a, [$FFC2]		; enemy Y pos
	sub c
	rla					; Put carry flag in lowest bit of A
	rlca				; Put carry flag in bit 1
	and a, $02
	or b
	ldh [hMarioState], a		; and OR it into FFC5
.checkBit6
	ld a, [wCommandArgument]
	bit 6, a			; Same but for X
	jr z, .checkBits2And3
	ld a, [wMarioPosX]		; Mario X
	ld c, a
	ldh a, [$FFC3]		; Enemy X
	ld b, a
	ldh a, [$FFCA]
	and a, $70			; width in tiles, times 16
	rrca
	rrca				; half width in pixels
	add b				; add to enemy X to get X coodinate of center of enemy
	sub c
	rla					; Put carry flag in lowest bit of A
	and a, $01
	ld b, a
	ldh a, [hMarioState]
	and a, $FE
	or b
	ldh [hMarioState], a		; And OR it into FFC5
.checkBits2And3
	ld a, [wCommandArgument]
	and a, $0C			; bits 2 and 3, invert corresponding direction
	jr z, .checkBits4and5
	rra
	rra
	ld b, a
	ldh a, [hMarioState]
	xor b				; invert the corresponding bits in FFC5
	ldh [hMarioState], a
.checkBits4and5
	ld a, [wCommandArgument]
	bit 5, a			; bit 5, replace Y direction with bit 1
	jr z, .checkBit4
	and a, $02
	or a, $FD			; put bit 1 in a bitmask with all other bits set
	ld b, a
	ldh a, [hMarioState]
	set 1, a
	and b				; and apply to FFC5
	ldh [hMarioState], a
.checkBit4
	ld a, [wCommandArgument]
	bit 4, a			; bit 4, replace X direction with bit 0
	jr z, .out
	and a, $01			; same, but with bit 0
	or a, $FE
	ld b, a
	ldh a, [hMarioState]
	set FACING_RIGHT_BIT, a
	and b
	ldh [hMarioState], a
.out
	pop hl
	jp .runScript

.checkF1
	cp a, $F1			; F1 - launch projectile
	jr nz, .checkF2
	ld a, $0A			; Temporarily store the current buffer
	call CopyBufferToEnemySlot
	call Call_24D6		; Overwrite current buffer. This makes sure the projectile
	ld a, $0A			; is spawned at the location of the enemy firing it
	call CopyEnemySlotToBuffer
	pop hl
	jp .runScript

.checkF2
	cp a, $F2			; F2 - set movement flags
	jr nz, .checkF3
	ld a, [wCommandArgument]
	ldh [$FFC7], a
	pop hl
	jp .runScript

.checkF3
	cp a, $F3			; F3 - Change ID and reinitialize
	jr nz, .checkF4
	ld a, [wCommandArgument]
	ldh [$FFC0], a
	cp a, $FF			; FF stands for an empty slot
	jp z, .enemyGone
	ld hl, $FFC0
	call InitEnemy
	pop hl
	ld hl, Data_349E	; reinitialize script
	ldh a, [$FFC0]
	rlca
	ld d, $00
	ld e, a
	add hl, de
	ldi a, [hl]
	ld e, a
	ld a, [hl]
	ld d, a
	ld h, d
	ld l, e
	jp .runScript

.checkF4
	cp a, $F4
	jr nz, .checkF5		; F4 - tick timer of some sort
	ld a, [wCommandArgument]
	ldh [$FFC9], a
	pop hl
	jp .runScript

.checkF5
	cp a, $F5			; F5 - Shoot with a 1 in 4 probability - Unused?
	jr nz, .checkF6
	ldh a, [rDIV]
	and a, $3			; "random" value from 0 to 3
	ld a, $F1
	jr z, .checkF1		; execute command F1 - launch projectile
	pop hl
	jp .runScript

.checkF6
	cp a, $F6			; F6 - Halt until Mario is close
	jr nz, .checkF7
	ld a, [wMarioPosX]		; Mario X
	ld b, a
	ldh a, [$FFC3]		; enemy X
	sub b
	add a, $14			; set carry flag if Mario is within [-$14, $20 - $14 - 1]
	cp a, $20			; pixels of enemy
	ld a, [wCommandArgument]
	dec a				; does not touch carry flag, but can set zero flag
	jr z, .checkCarry
	ccf					; invery carry flag if argument is not 1
.checkCarry
	jr c, .dontHalt
	ldh a, [$FFC4]		; put script index back to the start of this command
	dec a				; effectively halting the script until Mario is close
	dec a				; or conversely not close
	ldh [$FFC4], a
	pop hl
	ret

.dontHalt
	pop hl
	jp .runScript

.checkF7
	cp a, $F7			; F7 - Explode all enemies :)
	jr nz, .checkF9
	call ExplodeAllEnemies
	pop hl
	ret

.checkF9
	cp a, $F9			; F9 - Sound effect
	jr nz, .checkFA
	ld a, [wCommandArgument]
	ld [wNoiseSFX], a		; sound effect
	pop hl
	ret

.checkFA
	cp a, $FA			; FA - Sound effect
	jr nz, .checkFB
	ld a, [wCommandArgument]
	ld [wSquareSFX], a		; sound effect
	pop hl
	ret

.checkFB
	cp a, $FB			; FB - reset script until enemy is close enough
	jr nz, .checkFC
	ld a, [wCommandArgument]
	ld c, a
	ld a, [wMarioPosX]
	ld b, a
	ldh a, [$FFC3]
	sub b
	cp c
	jr c, .enemyClose
	xor a
	ldh [$FFC4], a
	pop hl
	jp .runScript

.enemyClose				; Unnecessary. Bug
	pop hl
	jp .runScript

.checkFC
	cp a, $FC			; FC - position enemy at the right of the screen
	jr nz, .checkFD		; Only used for Tatanga?
	ld a, [wCommandArgument]
	ldh [$FFC2], a
	ld a, $70
	ldh [$FFC3], a
	pop hl
	jp .runScript

.checkFD
	cp a, $FD			; FD - Music
	jr nz, .unknownCommand
	ld a, [wCommandArgument]
	ld [wActiveMusic], a
	pop hl
	ret

.unknownCommand			; silently ignore...
	pop hl
	jp .runScript

.enemyGone
	pop hl
	ret

.moveEnemy				; X movement first
	ldh a, [$FFC1]
	and a, $0F			; X speed
	jp z, .jmp_2975		; if zero, no point in doing collision detection
	ldh a, [hMarioState]
	bit FACING_RIGHT_BIT, a			; going right
	jr nz, .goingRight
	call Call_2B84		; some sort of collision detection. left bound?
	jr nc, .jmp_28CE
	ldh a, [$FFC7]
	bit 0, a			; set if enemy doesn't walk off edges
	jr z, .jmp_2896
	call Call_2BE4		; checks for collision bottom left bound, one tile down?
	jr c, .reverseAndGoRight	; carry means the tile isn't solid
.jmp_2896
	ldh a, [$FFC1]
	and a, $0F
	ld b, a
	ldh a, [$FFC3]		; X
	sub b
	ldh [$FFC3], a
	ldh a, [$FFCB]
	and a
	jp z, .jmp_2975
	ld a, [wMarioFacingDir]		; dir mario is facing?
	ld c, a
	push bc
	ld a, $20			; 20 if facing left
	ld [wMarioFacingDir], a
	call Call_1AAD		; Mario side collision
	pop bc
	and a
	jr nz, .jmp_28C7
	ld a, [wMarioPosX]		; Y pos
	sub b
	ld [wMarioPosX], a
	cp a, $0F
	jr nc, .jmp_28C7
	ld a, $0F
	ld [wMarioPosX], a
.jmp_28C7
	ld a, c
	ld [wMarioFacingDir], a
	jp .jmp_2975

.jmp_28CE
	ldh a, [$FFC7]
	and a, $0C			; test bits 2 and 3
	cp a, 0
	jr z, .jmp_2896		; bit 2 and 3 not set
	cp a, $4
	jr nz, .jmp_28E3
.reverseAndGoRight		; bit 2 set, bit 3 unset
	ldh a, [hMarioState]
	set FACING_RIGHT_BIT, a
	ldh [hMarioState], a
	jp .jmp_2975

.jmp_28E3
	cp a, $0C
	jp nz, .jmp_2975
	xor a
	ldh [$FFC4], a
	ldh [$FFC8], a
	jp .jmp_2975

.goingRight
	call Call_2B9A		; bottom right collision
	jr nc, .sideCollisionRight
	ldh a, [$FFC7]		; carry, so non-solid tile
	bit 0, a			; bit 0: don't walk off edges?
	jr z, .jmp_2900
	call Call_2BFE		; collision bottom right, one tile down
	jr c, .reverseAndGoLeft		; jump if not solid
.jmp_2900
	ldh a, [$FFC1]
	and a, $0F			; X speed
	ld b, a
	ldh a, [$FFC3]		; X
	add b
	ldh [$FFC3], a
	ldh a, [$FFCB]
	and a
	jr z, .jmp_2975
	ld a, [wMarioFacingDir]		; direction mario is facing
	ld c, a
	push bc
	xor a
	ld [wMarioFacingDir], a
	call Call_1AAD		; mario collision?
	pop bc
	and a
	jr nz, .jmp_2944
	ld a, [wMarioPosX]		; X pos
	add b
	ld [wMarioPosX], a
	cp a, $51
	jr c, .jmp_2944
	ld a, [$C0D2]
	cp a, $07
	jr nc, .jmp_294A
.run1
	ld a, [wMarioPosX]		; X pos
	sub a, $50
	ld b, a
	ld a, $50
	ld [wMarioPosX], a
	ldh a, [hScrollX]
	add b
	ldh [hScrollX], a
	call ScrollEnemiesByB		; scroll enemies
.jmp_2944
	ld a, c
	ld [wMarioFacingDir], a
	jr .jmp_2975

.jmp_294A
	ldh a, [hScrollX]
	and a, $0C			; 0000 1100
	jr nz, .run1
	ldh a, [hScrollX]
	and a, $FC			; 1111 1100
	ldh [hScrollX], a
	jr .jmp_2944

.sideCollisionRight
	ldh a, [$FFC7]
	and a, $0C			; test bit 2 and 3
	cp a, 0
	jr z, .jmp_2900		; neither bit set
	cp a, $4
	jr nz, .jmp_296C
.reverseAndGoLeft		; bit 2 set, bit 3 not set | at an edge
	ldh a, [hMarioState]
	res FACING_RIGHT_BIT, a			; moving left | reverse direction
	ldh [hMarioState], a
	jr .jmp_2975

.jmp_296C				; bit 2 and 3 set
	cp a, $0C
	jr nz, .jmp_2975
	xor a				; both bits set
	ldh [$FFC4], a		; reset script
	ldh [$FFC8], a
.jmp_2975				; 
	ldh a, [$FFC1]
	and a, $F0
	jp z, .jmp_29FD		; no Y speed, get out
	ldh a, [hMarioState]
	bit 1, a			; gravity
	jr nz, .jmp_29C1
	call Call_2C21		; upper left collision?
	jr nc, .jmp_29A1
.jmp_2987
	ldh a, [$FFC1]		; update Y position with Y speed
	and a, $F0
	swap a
	ld b, a
	ldh a, [$FFC2]
	sub b
	ldh [$FFC2], a
	ldh a, [$FFCB]
	and a
	jr z, .jmp_29FD
	ld a, [wMarioPosY]
	sub b				; if carrying Mario, add the displacement to his Y coord
	ld [wMarioPosY], a
	jr .jmp_29FD

.jmp_29A1
	ldh a, [$FFC7]
	and a, $C0				; test bits 6 and 7
	cp a, $00
	jr z, .jmp_2987			; jump if neither set
	cp a, $40				; test bit 6
	jp nz, .jmp_29B6		; could've been a JR, bug
	ldh a, [hMarioState]			; bit 6 set
	set 1, a				; moving down
	ldh [hMarioState], a
	jr .jmp_29FD

.jmp_29B6
	cp a, $C0
	jr nz, .jmp_29FD
	xor a
	ldh [$FFC4], a
	ldh [$FFC8], a
	jr .jmp_29FD

.jmp_29C1
	call CheckCollisionWithLowerTile		; collision one tile down
	jr nc, .jmp_29E0
.jmp_29C6
	ldh a, [$FFC1]
	and a, $F0			; Y speed
	swap a
	ld b, a
	ldh a, [$FFC2]
	add b
	ldh [$FFC2], a
	ldh a, [$FFCB]		; carrying Mario
	and a
	jr z, .jmp_29FD
	ld a, [wMarioPosY]
	add b				; if carrying, add to Mario's X position
	ld [wMarioPosY], a
	jr .jmp_29FD

.jmp_29E0
	ldh a, [$FFC7]
	and a, $30
	cp a, $00
	jr z, .jmp_29C6
	cp a, $10
	jr nz, .jmp_29F4
	ldh a, [hMarioState]
	res 1, a			; moving up
	ldh [hMarioState], a
	jr .jmp_29FD

.jmp_29F4
	cp a, $30
	jr nz, .jmp_29FD
	xor a
	ldh [$FFC4], a		; reset script
	ldh [$FFC8], a
.jmp_29FD
	xor a
	ldh [$FFCB], a
	ret

; stomp enemy
StombEnemy:: ; 2A01
	push hl
	ld a, [hl]
	ld e, a
	ld d, $00
	ld l, a
	ld h, $00
	sla e
	rl d
	sla e
	rl d
	add hl, de
	ld de, Data_3186
	add hl, de
	ld a, [hl]
	pop hl
	and a
	ret z
	push hl
	ld [hl], a
	call InitEnemy
	ld a, $FF
	pop hl
	ret

; enemy hit from down under
Call_2A23:: ; 2A23
	push hl
	ld a, [hl]
	ld e, a
	ld d, $00
	ld l, a
	ld h, $00
	sla e
	rl d
	sla e
	rl d
	add hl, de
	ld de, Data_3186
	add hl, de
	inc hl
	ld a, [hl]
	pop hl
	and a
	ret z
	ld [hl], a
	call InitEnemy
	ld a, $FF
	ret

; called when an enemy hits us on the side?
Call_2A44:: ; 2A44
	push hl
	ld a, [hl]
	ld e, a
	ld d, $00
	ld l, a
	ld h, $00
	sla e
	rl d
	sla e
	rl d
	add hl, de
	ld de, Data_3186
	add hl, de
	inc hl
	inc hl
	ld a, [hl]
	pop hl
	cp a, $FF
	ret z
	and a
	ret z
	ld [hl], a
	call InitEnemy
	xor a
	ret

; hit by superball
Call_2A68:: ; 2A68
	push hl
	ld a, l
	add a, $0C		; D1xC
	ld l, a
	ld a, [hl]
	and a, $3F		; health, like in 2AAD
	jr z, .jmp_2A89
	ld a, [hl]
	dec a
	ld [hl], a
	pop hl
	ld a, [hl]
	cp a, HIYOIHOI
	jr z, .bossHitSFX
	cp a, KING_TOTOMESU
	jr z, .bossHitSFX
	jr .jmp_2A86

.bossHitSFX
	ld a, $01
	ld [$DFF0], a	; creepy boss noise
.jmp_2A86
	ld a, $FE
	ret

.jmp_2A89
	pop hl
	push hl
	ld a, [hl]
	ld e, a
	ld d, $00
	ld l, a
	ld h, $00
	sla e
	rl d
	sla e
	rl d
	add hl, de
	ld de, Data_3186
	add hl, de
	inc hl
	inc hl
	inc hl
	ld a, [hl]
	pop hl
	and a
	ret z
	ld [hl], a
	call InitEnemy
	ld a, $FF
	ret

; enemy hit by bullet in autoscroll
Call_2AAD:: ; 2AAD
	push hl
	ld a, l
	add a, $0C		; "health"?
	ld l, a
	ld a, [hl]
	and a, $3F		; only the lower 6 bits are health
	jr z, .jmp_2AD9
	ld a, [hl]
	dec a
	ld [hl], a
	pop hl
	ld a, [hl]
	cp a, DRAGONZAMASU
	jr z, .bossHitSFX
	cp a, BIOKINTON
	jr z, .bossHitSFX
	cp a, TATANGA
	jr z, .explosionSFX
	jr .jmp_2AD6

.explosionSFX
	ld a, 1
	ld [wNoiseSFX], a	; explosion
	jr .jmp_2AD6

.bossHitSFX
	ld a, $01
	ld [$DFF0], a	; that weird scream bosses make when hit
.jmp_2AD6
	ld a, $FE		; not dead yet?
	ret

.jmp_2AD9
	pop hl
	push hl
	ld a, [hl]
	cp a, $60		; tatanga
	jr nz, .jmp_2AE3
	ld [$D007], a
.jmp_2AE3
	ld a, [hl]
	ld e, a
	ld d, $00
	ld l, a
	ld h, $00
	sla e
	rl d
	sla e
	rl d
	add hl, de
	ld de, Data_3186
	add hl, de
	inc hl
	inc hl
	inc hl
	inc hl
	ld a, [hl]
	pop hl
	and a
	ret z
	ld [hl], a
	call InitEnemy
	ld a, $FF		; dead
	ret

; HL refers to the slot of the enemy touched whilst invincible
Call_2B06:: ; 2B06
	push hl
	ld a, [hl]
	ld e, a
	ld d, $00
	ld l, a
	ld h, $00
	sla e
	rl d			; rotates possible carry in
	sla e
	rl d
	add hl, de		; HL = DE * 5
	ld de, Data_3186
	add hl, de
	inc hl
	inc hl
	inc hl
	inc hl
	ld a, [hl]
	pop hl
	and a
	ret z
	ld [hl], a
	call InitEnemy
	ld a, $FF
	ret

ExplodeAllEnemies:: ; 2B2A
	ld hl, $D100
.loop
	ld a, [hl]
	cp a, $FF
	jr z, .nextEnemySlot
	push hl
	ld [hl], $27	; 0 ID of mid air explosion
	inc hl			; 1
	inc hl			; 2 Y
	inc hl			; 3 X
	inc hl			; 4
	ld [hl], $00
	inc hl			; 5
	inc hl			; 6
	inc hl			; 7
	inc hl			; 8
	inc hl			; A dimensions
	ld [hl], $00
	inc hl			; B
	inc hl			; C health?
	ld [hl], $00
	pop hl
.nextEnemySlot
	ld a, l
	add a, $10
	ld l, a
	cp a, $A0
	jr c, .loop
	ld a, $27
	ldh [$FFC0], a
	xor a
	ldh [$FFC4], a
	ldh [$FFC7], a
	inc a
	ld [wNoiseSFX], a	; explosion sound
	ret

; enemy collision side check
Call_2B5D:: ; 2B5D
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	add a, $04
	ldh [$FFAE], a
	ld c, a
	ldh a, [hMarioState]		; 1 if facing right
	bit FACING_RIGHT_BIT, a
	jr .jmp_2B76

	ldh a, [$FFCA]
	and a, $70			; width
	rrca				; ...way more clever than the loop they usually use
	add c				; add the width to the X coordinate
	ldh [$FFAE], a
.jmp_2B76
	ldh a, [$FFC2]
	ldh [$FFAD], a
	call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret

; another collision check, but not taking into account width (just left check?)
; also doesn't add 4 like the previous one
Call_2B84:: ; 2B84
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	ldh [$FFAE], a
	ldh a, [$FFC2]
	ldh [$FFAD], a
	call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret

; collision check, adding width unconditionally (right bound?)
Call_2B9A:: ; 2B9A
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	add a, $8
	ld c, a
	ldh a, [$FFCA]
	and a, $70			; width in bits 4-6
	rrca				; A = width * 8, as there are 8 pixels per tile
	add c
	sub a, $8			; why is 8 added and subtracted?
	ldh [$FFAE], a
	ldh a, [$FFC2]
	ldh [$FFAD], a
	call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret

; checks collision one tile lower
CheckCollisionWithLowerTile:: ; 2BBB
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	add a, $4		; middle of leftmost tile?
	ldh [$FFAE], a
	ld c, a
	ldh a, [hMarioState]	; bit 0 on if facing right
	bit FACING_RIGHT_BIT, a
	jr .jmp_2BD4	; bug maybe? Should have been jr nz?

	ldh a, [$FFCA]	; mortality and dimensions?
	and a, $70
	rrca
	add c
	ldh [$FFAE], a
.jmp_2BD4
	ldh a, [$FFC2]
	add a, $08		; one tile lower
	ldh [$FFAD], a
	Call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret

; functionally identical to the previous one, apart from not clobbering C
; unused?
Call_2BE4:: ; 2BE4
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	add a, $03
	ldh [$FFAE], a
	ldh a, [$FFC2]
	add a, $08
	ldh [$FFAD], a
	call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret


; check for collision one tile down, 5 pixels to the right of the right bound?
; unused?
Call_2BFE:: ; 2BFE
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	add a, 5
	ld c, a
	ldh a, [$FFCA]
	and a, $70
	rrca
	add c
	sub a, 8		; to compensate for offset coordinates (Y-16, X-8)? todo
	ldh [$FFAE], a
	ldh a, [$FFC2]
	add a, 8
	ldh [$FFAD], a
	call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret

; top collision? upper left?
Call_2C21:: ; 2C21
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	add a, 4
	ldh [$FFAE], a
	ld c, a
	ldh a, [hMarioState]
	bit FACING_RIGHT_BIT, a
	jr .jmp_2C3A		; Should've been JR NZ?

	ldh a, [$FFCA]
	and a, $70			; 
	rrca
	add c
	ldh [$FFAE], a
.jmp_2C3A
	ldh a, [$FFCA]
	and a, 7			; height
	dec a
	swap a
	rrca				; multiply by 8
	ld c, a
	ldh a, [$FFC2]
	sub c
	ldh [$FFAD], a
	call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret

; Yet another collision detection routine, upper left bound?
Call_2C52:: ; 2C52
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	add a, 3
	ldh [$FFAE], a
	ldh a, [$FFCA]
	and a, 7
	dec a
	swap a
	rrca
	ld c, a
	ldh a, [$FFC2]
	sub c
	ldh [$FFAD], a
	Call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret

; another one. upper right bound?
Call_2C74:: ; 2C74
	ldh a, [$FFC3]
	ld c, a
	ldh a, [hScrollX]
	add c
	add a, 5
	ld c, a
	ldh a, [$FFCA]
	and a, $70
	rrca
	sub c
	sub a, 8
	ldh [$FFAE], a
	ldh a, [$FFCA]
	and a, 7
	dec a
	swap a
	rrca
	ld c, a
	ldh a, [$FFC2]
	sub c
	ldh [$FFAD], a
	Call LookupTile
	cp a, $5F
	ret c
	cp a, $F0
	ccf
	ret

; scroll all enemies by B
ScrollEnemiesByB:: ; 2C9F
	ld a, b
	and a
	ret z
	ldh a, [$FFC3]		; X
	sub b
	ldh [$FFC3], a
	push hl
	push de
	ld hl, $D103
	ld de, $0010
.loop
	ld a, [hl]
	sub b
	ld [hl], a
	add hl, de
	ld a, l
	cp a, $A0
	jr c, .loop
	pop de
	pop hl
	ret

; HL points to enemy slot (powerup slot?)
InitEnemy:: ; 2CBB
	push hl
	ld a, [hl]			; enemy ID
	ld d, 0
	ld e, a
	rlca
	add e				; times three
	rl d
	ld e, a
	ld hl, Data_3375
	add hl, de
	ldi a, [hl]
	ld b, a
	ldi a, [hl]
	ld d, a
	ld a, [hl]			; Store the data in B, D and A
	pop hl
	inc hl
	inc hl
	inc hl
	inc hl
	ld [hl], 0		; D1x4
	inc hl
	inc hl
	inc hl
	ld [hl], b			; D1x7 first byte
	inc hl				; some sort of behaviour? goomba is 6. koopa is 7
	ld [hl], 0		; D1x8
	inc hl
	ld [hl], 0		; D1x9
	inc hl
	ld [hl], d			; D1xA second byte
	inc hl				; hittable and dimensions
	inc hl
	ld [hl], a			; D1xC third byte
	ret

; Fill buffer from enemy slot
CopyEnemySlotToBuffer:: ; 2CE5
	swap a
	ld hl, $D100
	ld l, a
.fromHL
	ld de, $FFC0
	ld b, $0D			; bytes D, E, F are unused?
.loop
	ldi a, [hl]
	ld [de], a
	inc de
	dec b
	jr nz, .loop
	ret

; Fills enemy slot from buffer
CopyBufferToEnemySlot:: ; 2CF7
	swap a				; same as multiplying by 16. Slots are 16 bytes apart
	ld hl, $D100
	ld l, a
.toHL
	ld de, $FFC0
	ld b, $0D
.loop
	ld a, [de]
	ldi [hl], a
	inc de
	dec b
	jr nz, .loop
	ret

; instructions to build sprite?
; pointed to by tables at 2FE2 and 30B4
Data_2D09:: ; 2D09
    db   $90, $FF, $10, $90, $FF, $91, $FF, $20   ; $2D09
    db   $90, $FF, $40, $93, $48, $92, $FF, $40   ; $2D11
    db   $95, $48, $94, $FF, $97, $08, $96, $FF   ; $2D19
    db   $99, $08, $98, $FF, $10, $97, $18, $96   ; $2D21
    db   $FF, $10, $99, $18, $98, $FF, $9A, $FF   ; $2D29
    db   $20, $96, $28, $97, $FF, $10, $89, $11   ; $2D31
    db   $88, $18, $87, $FF, $10, $8C, $11, $8B   ; $2D39
    db   $18, $8A, $FF, $88, $01, $89, $0A, $87   ; $2D41
    db   $FF, $8B, $01, $8C, $0A, $8A, $FF, $10   ; $2D49
    db   $9C, $11, $8D, $FF, $8D, $01, $9C, $FF   ; $2D51
    db   $20, $8D, $21, $9C, $FF, $9B, $FF, $9D   ; $2D59
    db   $11, $9D, $FF, $9E, $11, $9E, $FF, $EF   ; $2D61
    db   $01, $EF, $01, $EF, $FF, $DD, $01, $DE   ; $2D69
    db   $FF, $20, $9D, $31, $9D, $0A, $9D, $11   ; $2D71
    db   $9D, $FF, $20, $9E, $31, $9E, $0A, $9E   ; $2D79
    db   $11, $9E, $FF, $83, $FF, $84, $FF, $85   ; $2D81
    db   $FF, $86, $FF, $40, $E0, $FF, $E5, $FF   ; $2D89
    db   $40, $F6, $FF, $40, $F7, $FF, $40, $F8   ; $2D91
    db   $FF, $FE, $FF, $DF, $FF, $40, $EE, $FF   ; $2D99
    db   $EF, $01, $EF, $FF, $B0, $01, $B1, $0A   ; $2DA1
    db   $A0, $01, $A1, $FF, $10, $B1, $11, $B0   ; $2DA9
    db   $1A, $A1, $11, $A0, $FF, $30, $C3, $31   ; $2DB1
    db   $C2, $3A, $D3, $31, $D2, $FF, $B2, $01   ; $2DB9
    db   $B3, $0A, $A2, $01, $A3, $FF, $10, $B3   ; $2DC1
    db   $11, $B2, $1A, $A3, $11, $A2, $FF, $B4   ; $2DC9
    db   $01, $B5, $0A, $A4, $01, $A5, $FF, $10   ; $2DD1
    db   $B5, $11, $B4, $1A, $A5, $11, $A4, $FF   ; $2DD9
    db   $B6, $01, $B7, $0A, $A6, $01, $A7, $FF   ; $2DE1
    db   $10, $B7, $11, $B6, $1A, $A7, $11, $A6   ; $2DE9
    db   $FF, $A8, $01, $A9, $FF, $10, $A9, $11   ; $2DF1
    db   $A8, $FF, $20, $A8, $21, $A9, $FF, $B8   ; $2DF9
    db   $01, $B9, $FF, $10, $B9, $11, $B8, $FF   ; $2E01
    db   $20, $B8, $21, $B9, $FF, $D0, $01, $D1   ; $2E09
    db   $0A, $C0, $01, $C1, $FF, $10, $D1, $11   ; $2E11
    db   $D0, $1A, $C1, $11, $C0, $FF, $D2, $01   ; $2E19
    db   $D3, $0A, $C2, $01, $C3, $FF, $10, $D3   ; $2E21
    db   $11, $D2, $1A, $C3, $11, $C2, $FF, $D4   ; $2E29
    db   $01, $D5, $0A, $C4, $01, $C5, $FF, $10   ; $2E31
    db   $D5, $11, $D4, $1A, $C5, $11, $C4, $FF   ; $2E39
    db   $D6, $01, $D7, $0A, $C6, $01, $C7, $FF   ; $2E41
    db   $10, $D7, $11, $D6, $1A, $C7, $11, $C6   ; $2E49
    db   $FF, $C8, $01, $C9, $FF, $10, $C9, $11   ; $2E51
    db   $C8, $FF, $20, $C8, $21, $C9, $FF, $D8   ; $2E59
    db   $01, $D9, $FF, $10, $D9, $11, $D8, $FF   ; $2E61
    db   $20, $D8, $21, $D9, $FF, $AC, $FF, $AE   ; $2E69
    db   $FF, $AF, $FF, $BD, $01, $BE, $01, $BF   ; $2E71
    db   $FF, $10, $BF, $11, $BE, $11, $BD, $FF   ; $2E79
    db   $BA, $FF, $BB, $FF, $C6, $01, $C7, $FF   ; $2E81
    db   $20, $C6, $21, $C7, $FF, $D6, $01, $D7   ; $2E89
    db   $FF, $20, $D6, $21, $D7, $FF, $40, $D0   ; $2E91
    db   $48, $C0, $FF, $40, $D1, $48, $C1, $FF   ; $2E99
    db   $BC, $08, $AC, $FF, $E2, $FF, $E3, $FF   ; $2EA1
    db   $C4, $01, $C5, $FF, $D4, $01, $D5, $FF   ; $2EA9
    db   $9F, $FF, $AA, $FF, $AD, $FF, $D8, $FF   ; $2EB1
    db   $D9, $FF, $BA, $01, $BB, $0A, $AA, $01   ; $2EB9
    db   $AB, $FF, $40, $E6, $FF, $BB, $0A, $AA   ; $2EC1
    db   $01, $AB, $11, $AA, $FF, $BB, $0A, $BA   ; $2EC9
    db   $01, $AB, $11, $BA, $FF, $BB, $0A, $AC   ; $2ED1
    db   $01, $AB, $11, $AC, $FF, $AC, $01, $AD   ; $2ED9
    db   $11, $AC, $FF, $20, $AC, $21, $AD, $31   ; $2EE1
    db   $AC, $FF, $DA, $01, $DB, $01, $DC, $0A   ; $2EE9
    db   $02, $CA, $01, $CB, $01, $CC, $01, $BA   ; $2EF1
    db   $0A, $02, $02, $CD, $01, $CE, $FF, $BB   ; $2EF9
    db   $01, $D6, $01, $D7, $0A, $02, $AB, $01   ; $2F01
    db   $C6, $01, $C7, $01, $AA, $0A, $02, $02   ; $2F09
    db   $CD, $01, $CE, $FF, $20, $AA, $31, $AA   ; $2F11
    db   $0A, $AA, $11, $AA, $FF, $20, $AB, $31   ; $2F19
    db   $AB, $0A, $AB, $11, $AB, $FF, $BC, $01   ; $2F21
    db   $BD, $0A, $CE, $01, $CF, $0A, $BE, $01   ; $2F29
    db   $BF, $0A, $AE, $01, $AF, $FF, $CC, $01   ; $2F31
    db   $CD, $0A, $DA, $01, $DB, $0A, $CA, $01   ; $2F39
    db   $CB, $0A, $BA, $01, $BB, $FF, $40, $88   ; $2F41
    db   $48, $87, $FF, $F9, $01, $FB, $FF, $F9   ; $2F49
    db   $01, $FA, $FF, $10, $F9, $12, $FB, $FF   ; $2F51
    db   $10, $F9, $12, $FA, $FF, $CE, $01, $CF   ; $2F59
    db   $0A, $BE, $01, $BF, $0A, $AE, $01, $AF   ; $2F61
    db   $FF, $CA, $01, $CB, $0A, $CC, $01, $CD   ; $2F69
    db   $0A, $BC, $01, $BD, $FF, $D0, $11, $D0   ; $2F71
    db   $0A, $C0, $11, $C0, $FF, $D1, $11, $D1   ; $2F79
    db   $0A, $C1, $11, $C1, $FF, $BA, $01, $BB   ; $2F81
    db   $01, $BC, $0A, $AB, $FF, $BD, $01, $BE   ; $2F89
    db   $01, $BF, $0A, $AE, $FF, $60, $93, $64   ; $2F91
    db   $92, $FF, $60, $95, $64, $94, $FF, $40   ; $2F99
    db   $CC, $41, $CD, $41, $CE, $41, $CF, $4A   ; $2FA1
    db   $42, $42, $BC, $41, $BD, $41, $BE, $41   ; $2FA9
    db   $BF, $4A, $42, $42, $AC, $41, $AD, $41   ; $2FB1
    db   $AE, $41, $AF, $4A, $CB, $FF, $40, $F2   ; $2FB9
    db   $41, $F3, $4A, $F0, $41, $F1, $FF, $8C   ; $2FC1
    db   $01, $8D, $01, $9C, $0A, $02, $89, $01   ; $2FC9
    db   $8A, $01, $8B, $FF, $DA, $01, $C8, $01   ; $2FD1
    db   $C9, $0A, $02, $CA, $01, $DB, $01, $DC   ; $2FD9
    db   $FF                                      ; $2FE1

; 0x69 pointers for data if facing left
Data_2FE2:: ; 2FE2
    db $09, $2D, $0B, $2D, $0E, $2D, $10, $2D, $13, $2D, $18, $2D, $25, $2D, $2A, $2D
    db $2F, $2D, $31, $2D, $36, $2D, $3D, $2D, $50, $2D, $59, $2D, $5E, $2D, $60, $2D
    db $64, $2D, $68, $2D, $68, $2D, $6E, $2D, $72, $2D, $7B, $2D, $84, $2D, $86, $2D
    db $88, $2D, $8A, $2D, $8C, $2D, $8F, $2D, $91, $2D, $94, $2D, $97, $2D, $9A, $2D
    db $9C, $2D, $9E, $2D, $A1, $2D, $C6, $2E, $CE, $2E, $D6, $2E, $DE, $2E, $E4, $2E
    db $AD, $2D, $C7, $2D, $D8, $2D, $E9, $2D, $F6, $2D, $FB, $2D, $04, $2E, $09, $2E
    db $16, $2E, $27, $2E, $38, $2E, $49, $2E, $56, $2E, $5B, $2E, $64, $2E, $69, $2E
    db $6E, $2E, $70, $2E, $72, $2E, $7A, $2E, $81, $2E, $83, $2E, $85, $2E, $89, $2E
    db $8E, $2E, $92, $2E, $97, $2E, $9C, $2E, $A1, $2E, $A5, $2E, $A7, $2E, $B6, $2D
    db $EB, $2E, $00, $2F, $A9, $2E, $AD, $2E, $15, $2F, $1E, $2F, $27, $2F, $37, $2F
    db $B1, $2E, $47, $2F, $54, $2F, $59, $2F, $5E, $2F, $6A, $2F, $76, $2F, $7E, $2F
    db $9A, $2D, $72, $2E, $B3, $2E, $6E, $2E, $B5, $2E, $86, $2F, $8E, $2F, $96, $2F
    db $9B, $2F, $B7, $2E, $B9, $2E, $A0, $2F, $BB, $2E, $BF, $2F, $C8, $2F, $D5, $2F
    db $C3, $2E

; 0x69 pointers for data if facing left
Data_30B4:: ; 30B4
    db $09, $2D, $0B, $2D, $0E, $2D, $10, $2D, $13, $2D, $18, $2D, $1D, $2D, $21, $2D
    db $2F, $2D, $31, $2D, $44, $2D, $4A, $2D, $55, $2D, $59, $2D, $5E, $2D, $60, $2D
    db $64, $2D, $68, $2D, $68, $2D, $6E, $2D, $72, $2D, $7B, $2D, $84, $2D, $86, $2D
    db $88, $2D, $8A, $2D, $8C, $2D, $8F, $2D, $91, $2D, $94, $2D, $97, $2D, $9A, $2D
    db $9C, $2D, $9E, $2D, $A1, $2D, $C6, $2E, $CE, $2E, $D6, $2E, $DE, $2E, $E4, $2E
    db $A5, $2D, $BF, $2D, $D0, $2D, $E1, $2D, $F2, $2D, $FB, $2D, $00, $2E, $09, $2E
    db $0E, $2E, $1F, $2E, $30, $2E, $41, $2E, $52, $2E, $5B, $2E, $60, $2E, $69, $2E
    db $6E, $2E, $70, $2E, $72, $2E, $74, $2E, $81, $2E, $83, $2E, $85, $2E, $89, $2E
    db $8E, $2E, $92, $2E, $97, $2E, $9C, $2E, $A1, $2E, $A5, $2E, $A7, $2E, $B6, $2D
    db $EB, $2E, $00, $2F, $A9, $2E, $AD, $2E, $15, $2F, $1E, $2F, $27, $2F, $37, $2F
    db $B1, $2E, $47, $2F, $4C, $2F, $50, $2F, $5E, $2F, $6A, $2F, $76, $2F, $7E, $2F
    db $9A, $2D, $72, $2E, $B3, $2E, $6E, $2E, $B5, $2E, $86, $2F, $8E, $2F, $96, $2F
    db $9B, $2F, $B7, $2E, $B9, $2E, $A0, $2F, $BB, $2E, $BF, $2F, $C8, $2F, $D5, $2F
    db $C3, $2E

; 0x63 entries of 5 bytes
Data_3186::; 3186
    db $01, $11, $FF, $11, $11, $00, $00, $00, $00, $00, $00, $00, $FF, $FF, $FF, $00
    db $00, $00, $00, $00, $05, $12, $FF, $12, $12, $00, $00, $00, $00, $00, $00, $00
    db $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $4F, $4F, $FF, $FF, $FF
    db $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00
    db $00, $00, $00, $00, $00, $00, $0F, $15, $FF, $15, $15, $00, $00, $00, $00, $00
    db $FF, $00, $FF, $00, $27, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $14
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $1C, $19
    db $FF, $19, $19, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00
    db $00, $00, $00, $00, $FF, $00, $4F, $00, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $FF, $00, $27, $00, $00, $FF, $00, $00, $00, $00, $FF, $00, $00
    db $FF, $00, $FF, $21, $21, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00
    db $00, $FF, $00, $00, $FF, $00, $FF, $00, $27, $1C, $19, $FF, $19, $19, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $28, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $2A, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $27, $27
    db $00, $00, $FF, $27, $27, $40, $41, $FF, $41, $41, $00, $00, $FF, $4F, $4F, $00
    db $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $37, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $3D, $3E, $FF, $3E
    db $3E, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $40, $41, $FF, $41, $41
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $43, $44, $FF, $44, $44, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $00, $00
    db $FF, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $FF, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $0D, $0D, $FF, $00, $0D, $4D, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    db $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $FF, $FF, $FF, $FF, $27, $FF
    db $FF, $FF, $FF, $27, $00, $00, $FF, $00, $00, $00, $00, $FF, $FF, $FF, $57, $15
    db $FF, $15, $15, $00, $00, $00, $00, $00, $00, $00, $FF, $00, $00, $FF, $FF, $FF
    db $FF, $27, $00, $00, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00
    db $FF, $00, $00, $FF, $00, $FF, $00, $00, $FF, $00, $FF, $00, $00, $FF, $00, $FF
    db $00, $00, $FF, $00, $4F, $00, $00, $FF, $00, $62, $00, $00, $00, $00, $00

; 0x63 entries of 3 bytes
Data_3375:: ; 3375
    db $06, $11, $00, $02, $11, $00, $01, $11, $00, $00, $11, $00, $07, $11, $00, $02
    db $11, $00, $00, $22, $00, $00, $91, $00, $09, $33, $C4, $07, $22, $81, $00, $B1
    db $00, $00, $B1, $00, $00, $A1, $00, $00, $00, $00, $34, $22, $41, $02, $21, $00
    db $00, $12, $00, $00, $11, $00, $00, $11, $00, $00, $91, $00, $08, $91, $00, $00
    db $21, $00, $07, $22, $40, $00, $21, $00, $00, $21, $00, $00, $22, $00, $50, $24
    db $D3, $00, $21, $00, $02, $21, $00, $00, $22, $41, $00, $21, $00, $00, $11, $00
    db $00, $22, $82, $00, $22, $00, $00, $11, $00, $00, $11, $00, $00, $22, $41, $E0
    db $22, $41, $00, $11, $00, $00, $22, $00, $24, $11, $00, $06, $11, $00, $24, $11
    db $00, $06, $11, $00, $04, $11, $00, $00, $11, $00, $00, $11, $00, $00, $21, $00
    db $00, $21, $00, $06, $22, $40, $00, $23, $C9, $00, $A2, $00, $0C, $11, $00, $00
    db $91, $00, $00, $91, $00, $00, $91, $00, $00, $B1, $00, $00, $B1, $00, $00, $A1
    db $00, $00, $A1, $00, $30, $12, $82, $02, $11, $00, $00, $11, $00, $00, $22, $80
    db $02, $21, $00, $00, $21, $00, $00, $22, $80, $02, $21, $00, $00, $21, $00, $00
    db $12, $00, $00, $22, $00, $34, $A2, $00, $54, $22, $FF, $00, $92, $00, $00, $11
    db $00, $00, $11, $40, $00, $91, $00, $00, $91, $00, $00, $91, $00, $00, $22, $00
    db $00, $11, $00, $00, $11, $00, $00, $22, $40, $00, $22, $40, $00, $11, $00, $00
    db $11, $40, $B4, $22, $81, $02, $21, $00, $00, $11, $00, $00, $11, $89, $00, $11
    db $00, $00, $43, $C0, $00, $11, $32, $00, $11, $02, $00, $11, $05, $00, $11, $02
    db $00, $43, $D8, $54, $32, $D3, $54, $22, $00

; 0x63 pointers to Data_3564
Data_349E:: ; 349E
    db $64, $35, $6E, $35, $76, $35, $9D, $35, $AA, $35, $B4, $35, $D3, $35, $E3, $35
    db $ED, $35, $0B, $36, $38, $36, $4A, $36, $5C, $36, $74, $36, $93, $36, $C9, $36
    db $D1, $36, $FB, $36, $FF, $36, $03, $37, $08, $37, $1A, $37, $20, $37, $3B, $37
    db $58, $37, $65, $37, $6B, $37, $7B, $37, $83, $37, $8D, $37, $99, $37, $A1, $37
    db $AA, $37, $B6, $37, $CD, $37, $D7, $37, $EB, $37, $1D, $38, $6E, $38, $70, $38
    db $7F, $38, $9A, $38, $A1, $38, $BC, $38, $C3, $38, $CD, $38, $DA, $38, $E2, $38
    db $F7, $38, $39, $39, $42, $39, $5B, $39, $64, $39, $83, $39, $99, $39, $9D, $39
    db $B1, $39, $C2, $39, $D3, $39, $E5, $39, $F7, $39, $38, $3A, $40, $3A, $46, $3A
    db $5D, $3A, $65, $3A, $6B, $3A, $8F, $3A, $97, $3A, $9D, $3A, $A4, $3A, $CA, $3A
    db $F1, $3A, $FB, $3A, $18, $3B, $22, $3B, $2A, $3B, $2E, $3B, $3E, $3B, $45, $3B
    db $51, $3B, $57, $3B, $70, $3B, $83, $3B, $C5, $3B, $E0, $3B, $07, $3C, $33, $3C
    db $3E, $3C, $46, $3C, $63, $3C, $6D, $3C, $81, $3C, $9A, $3C, $AF, $3C, $C3, $3C
    db $D8, $3C, $ED, $3C, $05, $3D

; pointed to by table at 349E
; enemy scripts
Data_3564:: ; 3564
    db $F8, $00, $F4, $02, $01, $E2, $F8, $01, $E3, $FF, $F8, $02, $00, $EF, $EF, $EF
    db $F3, $FF, $F0, $20, $F8, $04, $00, $EF, $F6, $00, $10, $EF, $00, $EF, $F8, $05
    db $EF, $F8, $04, $EF, $F8, $05, $EF, $F8, $04, $EF, $F8, $05, $EF, $F8, $04, $EF
    db $F0, $22, $10, $EF, $00, $EF, $EF, $EF, $FF, $F8, $1F, $F4, $02, $00, $EF, $EF
    db $F1, $47, $EF, $EF, $EF, $FF, $F4, $02, $F8, $06, $01, $E2, $F8, $07, $E3, $FF
    db $F8, $08, $00, $EF, $EF, $F8, $0E, $E4, $F8, $08, $E4, $F8, $0E, $E4, $F8, $08
    db $E4, $F8, $0E, $E4, $F8, $08, $E4, $F8, $0E, $E4, $F8, $08, $E4, $F3, $46, $F8
    db $65, $F0, $22, $F4, $01, $10, $EF, $F0, $20, $10, $EF, $00, $EF, $E8, $FF, $F8
    db $68, $F4, $01, $F0, $20, $10, $E2, $F3, $13, $F0, $30, $F4, $01, $F8, $48, $00
    db $EF, $F8, $49, $00, $EF, $F9, $04, $F1, $1B, $E8, $10, $EF, $E4, $F9, $04, $F1
    db $1B, $F0, $22, $10, $EF, $E4, $FF, $F4, $03, $F8, $56, $01, $E2, $F8, $57, $E2
    db $F8, $56, $E2, $F8, $57, $E2, $F8, $56, $E2, $F8, $57, $E2, $F8, $56, $E2, $F8
    db $57, $E2, $F8, $56, $E2, $F8, $57, $E2, $F8, $56, $00, $E8, $F8, $57, $F9, $04
    db $F1, $51, $E8, $FF, $F8, $12, $F4, $01, $F0, $22, $10, $EE, $EF, $EF, $EF, $F0
    db $20, $EF, $EF, $EF, $EF, $FF, $F8, $12, $F4, $01, $F0, $10, $01, $EE, $EF, $EF
    db $E8, $F0, $11, $EF, $EF, $EF, $E8, $FF, $F8, $13, $F0, $22, $F4, $0F, $00, $EA
    db $F4, $00, $10, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $FF
    db $F0, $64, $11, $E5, $01, $11, $01, $11, $01, $01, $F0, $22, $01, $01, $11, $01
    db $11, $01, $11, $E5, $20, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $FF, $F8
    db $28, $F0, $60, $F4, $02, $00, $E4, $F8, $29, $E4, $F8, $28, $E4, $F8, $29, $E4
    db $F8, $28, $F0, $60, $42, $42, $22, $22, $F8, $29, $12, $12, $12, $02, $F0, $22
    db $F8, $28, $02, $12, $12, $12, $F8, $29, $22, $22, $42, $42, $F8, $28, $41, $EF
    db $EF, $EF, $EF, $EF, $FF, $F8, $2C, $00, $EF, $EF, $EF, $F3, $15, $F8, $42, $F0
    db $20, $20, $EE, $F8, $43, $EF, $F8, $42, $EF, $F8, $43, $E8, $10, $E7, $00, $E3
    db $F8, $42, $F0, $22, $00, $E3, $10, $E7, $20, $E7, $F8, $43, $EF, $F8, $42, $EF
    db $F8, $43, $EF, $00, $EF, $EF, $FF, $F8, $03, $F3, $0D, $F8, $09, $F3, $0D, $F8
    db $68, $00, $EF, $FF, $F8, $68, $F0, $20, $F4, $01, $10, $EE, $EF, $EF, $EF, $E8
    db $F4, $0F, $00, $EF, $F3, $FF, $F9, $03, $F8, $2D, $F3, $0D, $F4, $02, $F0, $40
    db $F8, $32, $01, $E8, $F8, $33, $E9, $F8, $32, $E9, $F8, $33, $E9, $F8, $32, $E9
    db $F8, $33, $E9, $F1, $17, $F3, $18, $F8, $3E, $70, $F0, $20, $10, $EE, $E8, $F0
    db $40, $11, $EE, $01, $EE, $F0, $22, $11, $EE, $10, $E7, $F0, $04, $11, $EE, $01
    db $EE, $EF, $F3, $FF, $F8, $40, $00, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $F3
    db $16, $F9, $03, $F8, $37, $F3, $0D, $F8, $4F, $10, $EF, $EF, $E8, $F8, $4E, $00
    db $E8, $F9, $04, $F1, $1F, $EF, $FF, $F0, $30, $F8, $1F, $37, $44, $F3, $1E, $F4
    db $01, $F8, $36, $00, $EF, $EF, $EF, $F3, $19, $F8, $2A, $F0, $90, $F4, $02, $12
    db $E7, $F8, $2B, $E8, $FF, $F8, $4A, $01, $E7, $F8, $4B, $E8, $FF, $F8, $1F, $F0
    db $30, $70, $70, $40, $F3, $58, $F8, $28, $F0, $08, $F4, $02, $10, $E3, $F8, $29
    db $E4, $FF, $F0, $22, $40, $F1, $22, $F0, $20, $50, $50, $60, $F1, $22, $F8, $14
    db $00, $EF, $F8, $15, $EF, $F8, $1F, $F3, $FF, $F8, $45, $F4, $01, $01, $E3, $F8
    db $46, $E4, $FF, $F8, $45, $F0, $C0, $F4, $01, $12, $EF, $EF, $EF, $EF, $EF, $EF
    db $EF, $EF, $EF, $EF, $EF, $F3, $FF, $F8, $2A, $F0, $60, $20, $EE, $F8, $2B, $EF
    db $F8, $2A, $F0, $60, $EF, $F8, $2B, $E8, $10, $E7, $00, $E3, $F8, $2A, $F0, $62
    db $F9, $04, $F1, $23, $00, $E3, $10, $E7, $20, $E7, $F8, $2B, $EF, $F8, $2A, $F0
    db $40, $EF, $F8, $2B, $EF, $00, $EF, $EF, $FF, $F8, $32, $F0, $22, $00, $EF, $EF
    db $F4, $00, $F6, $01, $10, $E8, $F8, $33, $E8, $F8, $32, $E8, $F8, $33, $E8, $F8
    db $32, $E8, $F8, $33, $E8, $F8, $32, $E8, $F8, $33, $E8, $F8, $32, $E8, $F8, $33
    db $E8, $F8, $32, $F0, $20, $F4, $01, $E8, $F8, $33, $E8, $F8, $32, $E8, $F8, $33
    db $E8, $F8, $32, $E8, $F8, $33, $E8, $F8, $32, $E8, $F8, $33, $E8, $F8, $32, $E8
    db $F8, $33, $E8, $F8, $32, $E8, $F8, $33, $E8, $FF, $F3, $FF, $F8, $14, $00, $E7
    db $F8, $15, $E8, $F8, $14, $E8, $F8, $15, $E8, $F3, $FF, $F4, $01, $F8, $16, $F0
    db $31, $70, $11, $E5, $01, $11, $01, $11, $01, $01, $F0, $22, $01, $01, $11, $01
    db $11, $01, $11, $E5, $F3, $29, $F8, $16, $F4, $01, $01, $EF, $FF, $F4, $01, $F8
    db $17, $F0, $31, $70, $11, $E5, $01, $11, $01, $11, $01, $01, $F0, $22, $01, $01
    db $11, $01, $11, $01, $11, $E5, $F3, $2B, $F8, $17, $F4, $01, $01, $EF, $FF, $F4
    db $01, $F8, $19, $F0, $31, $40, $40, $F3, $34, $F4, $02, $F0, $20, $F8, $1A, $10
    db $E3, $F8, $1B, $E3, $F3, $2E, $F8, $1A, $00, $E8, $F8, $1B, $E8, $FF, $F8, $2C
    db $F0, $91, $F4, $01, $01, $EF, $41, $F1, $30, $01, $EF, $41, $F1, $30, $01, $EF
    db $41, $F3, $30, $F0, $10, $F8, $2C, $01, $EE, $F8, $2E, $EF, $F8, $2C, $EF, $F8
    db $2E, $EF, $F8, $2C, $EF, $F8, $2E, $EF, $F8, $2C, $EF, $F8, $2E, $E8, $F0, $80
    db $F8, $2C, $10, $EF, $F0, $11, $F8, $2E, $10, $EF, $F8, $2C, $02, $EE, $F8, $2E
    db $EF, $F8, $2C, $EF, $F8, $2E, $EF, $F8, $2C, $EF, $F8, $2E, $EF, $F8, $2C, $EF
    db $F8, $2E, $EF, $F3, $FF, $F4, $04, $F8, $2A, $06, $F8, $2B, $06, $FF, $F8, $54
    db $F4, $01, $F1, $33, $F0, $04, $00, $E7, $F8, $55, $07, $05, $00, $EF, $EF, $EF
    db $F0, $40, $F8, $54, $07, $05, $FF, $F8, $1F, $F0, $30, $72, $72, $42, $F3, $47
    db $F8, $19, $F4, $02, $F0, $20, $31, $21, $E4, $11, $E4, $01, $11, $01, $F0, $22
    db $01, $11, $01, $11, $E4, $21, $E4, $31, $30, $EF, $EF, $EF, $EF, $EF, $FF, $F8
    db $20, $F0, $22, $F4, $01, $F6, $01, $10, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF
    db $EF, $EF, $EF, $EF, $FF, $F8, $21, $00, $FF, $F8, $21, $F0, $22, $00, $E7, $10
    db $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $FF, $F8, $12, $F0
    db $32, $F4, $02, $11, $EE, $EF, $E8, $F0, $31, $11, $EE, $EF, $E8, $FF, $F8, $12
    db $F0, $30, $F4, $02, $11, $EE, $EF, $E8, $F0, $33, $11, $EE, $EF, $E8, $FF, $F8
    db $22, $F4, $01, $F0, $22, $10, $EE, $EF, $EF, $EF, $F0, $20, $EF, $EF, $EF, $EF
    db $FF, $F8, $22, $F4, $01, $F0, $10, $01, $EE, $EF, $EF, $E8, $F0, $11, $EF, $EF
    db $EF, $E8, $FF, $F8, $23, $F4, $01, $F0, $60, $31, $21, $E2, $F8, $24, $E4, $F8
    db $25, $E2, $11, $E1, $F8, $24, $E4, $11, $F8, $23, $00, $11, $00, $11, $F8, $24
    db $00, $01, $F0, $22, $01, $00, $F8, $25, $11, $00, $11, $00, $F8, $24, $11, $E3
    db $F8, $23, $E3, $21, $F8, $24, $E4, $F8, $25, $E4, $F8, $24, $31, $40, $EF, $EF
    db $EF, $EF, $EF, $FF, $F8, $26, $00, $EF, $EF, $EF, $F3, $3E, $F9, $03, $F8, $27
    db $F3, $0D, $F8, $2A, $F0, $40, $00, $EF, $EF, $EF, $EF, $F0, $40, $EF, $EF, $F8
    db $2B, $F9, $04, $F1, $23, $EF, $EF, $EF, $FF, $F8, $2E, $00, $EF, $EF, $EF, $F3
    db $41, $F9, $03, $F8, $2F, $F3, $0D, $F8, $30, $F0, $10, $01, $E8, $F8, $31, $E8
    db $F8, $30, $E8, $F8, $31, $E8, $F8, $30, $E8, $F8, $31, $00, $E8, $F8, $30, $E8
    db $F8, $31, $F1, $45, $E8, $F8, $30, $E8, $F8, $31, $FF, $F8, $34, $00, $EF, $EF
    db $EF, $F3, $44, $F9, $03, $F8, $35, $F3, $0D, $F0, $22, $F8, $44, $10, $EF, $FF
    db $F0, $10, $04, $F9, $01, $F8, $0F, $00, $E4, $F8, $10, $E4, $F8, $0F, $E4, $F8
    db $10, $E4, $F8, $0F, $E4, $F8, $10, $E4, $F8, $0F, $E4, $F8, $10, $E4, $F8, $0F
    db $E4, $F8, $10, $E4, $F3, $FF, $F8, $31, $F0, $20, $F4, $02, $42, $42, $22, $22
    db $F8, $47, $12, $12, $12, $02, $F0, $22, $F8, $31, $02, $12, $12, $12, $F8, $47
    db $22, $22, $42, $42, $F8, $31, $41, $EF, $EF, $EF, $EF, $EF, $FF, $F4, $01, $F8
    db $4C, $11, $EE, $F8, $4D, $EF, $FF, $F8, $51, $F0, $20, $00, $EF, $10, $EF, $00
    db $EF, $EF, $EF, $EF, $EF, $FA, $09, $F1, $4A, $EF, $EF, $F0, $22, $10, $EF, $00
    db $EF, $EF, $EF, $FF, $F8, $1F, $F0, $30, $70, $30, $F0, $40, $F3, $4B, $F8, $52
    db $01, $E7, $F8, $53, $E8, $FF, $F8, $21, $00, $FF, $F8, $21, $F0, $20, $00, $E7
    db $10, $EE, $EF, $EF, $EF, $EF, $EF, $EF, $F3, $4E, $F8, $21, $F0, $11, $02, $EF
    db $FF, $F9, $01, $F8, $14, $00, $EF, $F8, $15, $EF, $F9, $01, $F7, $F8, $1F, $F0
    db $91, $F3, $5A, $F8, $46, $F0, $31, $74, $F4, $00, $20, $EF, $E8, $10, $EF, $F0
    db $22, $10, $EF, $20, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $FF, $F8, $31, $F0, $10
    db $F4, $01, $02, $EF, $EF, $E4, $F0, $90, $22, $EA, $FA, $09, $F1, $50, $FF, $F8
    db $33, $F0, $90, $F4, $01, $02, $E3, $F8, $32, $E4, $FB, $20, $F8, $32, $41, $41
    db $41, $31, $F8, $33, $31, $31, $11, $21, $F8, $32, $11, $11, $11, $F8, $33, $11
    db $01, $01, $F0, $08, $F8, $32, $01, $01, $11, $F8, $33, $11, $11, $11, $F8, $32
    db $21, $11, $31, $31, $F8, $33, $31, $41, $41, $41, $02, $EF, $EF, $EF, $EF, $EF
    db $FF, $F8, $45, $F4, $03, $41, $41, $41, $43, $33, $23, $24, $14, $04, $F0, $08
    db $14, $14, $14, $34, $33, $32, $42, $41, $40, $F0, $04, $FF, $F8, $5F, $F0, $22
    db $F8, $5F, $00, $EF, $10, $EF, $00, $EF, $F8, $60, $EF, $F8, $5F, $EF, $F8, $60
    db $EF, $F8, $5F, $EF, $F8, $60, $EF, $F8, $5F, $EF, $F0, $20, $10, $EF, $00, $EF
    db $EF, $EF, $FF, $F8, $28, $F0, $60, $F4, $02, $00, $E2, $F8, $29, $E2, $F8, $28
    db $E2, $F8, $29, $E2, $F8, $28, $F0, $60, $42, $32, $32, $22, $12, $12, $F0, $22
    db $12, $22, $22, $32, $32, $42, $F8, $28, $41, $EF, $EF, $EF, $EF, $EF, $FF, $F4
    db $02, $F8, $2C, $00, $EF, $EF, $EF, $EF, $F3, $56, $F8, $45, $01, $E3, $F8, $46
    db $E4, $FF, $F0, $08, $F4, $02, $F8, $61, $10, $F8, $62, $10, $F8, $61, $10, $F8
    db $62, $10, $F8, $61, $10, $F8, $62, $10, $F8, $61, $10, $F8, $62, $10, $FF, $F4
    db $01, $F8, $52, $12, $E7, $F8, $53, $E8, $FF, $FD, $13, $F8, $1F, $F4, $0F, $00
    db $EF, $F8, $63, $F0, $20, $F4, $02, $10, $EF, $EF, $EF, $F3, $60, $F8, $1F, $F0
    db $10, $04, $F8, $64, $F0, $20, $F4, $01, $01, $E8, $F4, $00, $F8, $1F, $F1, $5F
    db $40, $F1, $5E, $40, $F3, $5D, $F8, $45, $F0, $30, $F4, $01, $21, $EA, $F4, $00
    db $E8, $02, $E8, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $FF, $F8, $45, $F0, $30, $F4
    db $01, $01, $EA, $F4, $00, $E8, $02, $EF, $EF, $EF, $EF, $EF, $EF, $EF, $FF, $F8
    db $45, $F0, $32, $F4, $01, $21, $EA, $F4, $00, $E8, $02, $E8, $EF, $EF, $EF, $EF
    db $EF, $EF, $EF, $FF, $F8, $63, $F1, $5C, $F0, $90, $10, $EF, $EF, $01, $EF, $F1
    db $5C, $F0, $91, $10, $EF, $E8, $01, $EF, $FF, $F4, $01, $F8, $66, $11, $E3, $F8
    db $67, $E4, $F8, $66, $E4, $F8, $67, $E4, $F8, $66, $E4, $F8, $67, $E4, $F1, $53
    db $FF, $F9, $01, $F8, $14, $00, $E7, $F8, $15, $E8, $F8, $14, $E8, $F8, $15, $E8
    db $F8, $1F, $FC, $A8, $F3, $5B

; called at level start, is some sort of init
Call_3D1A; 3D1A
	ld hl, wOAMBuffer + $30	; TODO ? wOAMBuffer + $30 ? used for "dynamic" sprites?
	ld b, $20
	xor a
.jmp_3D20
	ldi [hl], a
	dec b
	jr nz, .jmp_3D20
	ld hl, wGameTimer
	ld a, $28		; 40 frames per time unit
	ldi [hl], a		; DA00 - Time hundredths	
	xor a
	ldi [hl], a		; DA01 - Time ones and tens
	ld a, 4
	ldi [hl], a		; DA02 - Time hundreds
	call DisplayTimer.printTimer	; TODO
	ld a, $20		; Some sort of timers? For "dynamic" sprites?
	ldi [hl], a		; DA03
	ldi [hl], a		; DA04
	ldi [hl], a		; DA05
	ldi [hl], a		; DA06
	ld a, $F6		; One for each "timer"?
	ldi [hl], a		; DA07
	ldi [hl], a		; DA08
	ldi [hl], a		; DA09
	ldi [hl], a		; DA0A
	ld a, $30		; Cycles between the three "timers"
	ldi [hl], a		; DA0B
	xor a
	ld b, 9
.loop
	ldi [hl], a		; DA0C - DA14
	dec b
	jr nz, .loop
	ld a, 2
	ldi [hl], a		; DA15 - Lives
	dec a
	ldi [hl], a		; DA16 - TODO
	xor a
	ldi [hl], a		; DA17 - wPrizeAwarded
	ldi [hl], a		; DA18
	ldi [hl], a		; DA19
	ldi [hl], a		; DA1A
	ld a, $40
	ldi [hl], a		; DA1B - wBonusGameEndTimer
	xor a
	ldi [hl], a		; DA1C
	ldi [hl], a		; DA1D - wGameTimerExpiringFlag
	ldi [hl], a		; DA1E - wBonusGameGrowAnimationFlag
	ld a, $40
	ldi [hl], a		; DA1F - wBonusGameAnimationTimer
	xor a
	ld b, $08
.loop2
	ldi [hl], a		; DA20 - DA27 - ... to here
	dec b
	jr nz, .loop2
	ld a, $04
	ldi [hl], a		; DA28
	ld a, $11
	ld [hl], a		; DA29 - Changes during the bonus game..
	ret

DisplayTimer:: ; 3D6A ; TODO better name?
	ld a, [$C0A4]		; stores game over?
	and a
	ret nz
	ldh a, [hGameState]
	cp a, STATE_GOTO_BONUS_GAME			; game states > $12 don't make the timer count TODO
	ret nc
	ld a, [wGameTimer]	; Timer subdivision
	cp a, 40			; 40 frames per time unit (why not 60?)
	ret nz
	call .printTimer	; well, that's silly, could've just fallen through. Bug?
	ret

.printTimer ; 3D7E
	ld de, vBGMap0 + SCRN_VX_B + 19 ; $9833
	ld a, [wGameTimer + 1]	; Ones and Tens
	ld b, a
	and a, $0F
	ld [de], a
	dec e
	ld a, b
	and a, $F0
	swap a
	ld [de], a
	dec e
	ld a, [wGameTimer + 2]	; Hundreds
	and a, $0F
	ld [de], a
	ret

; entering bonus game. Clear the background, and print amount of lives
HandleGotoBonusGame:: ; 3D97
	ld hl, wActiveMusic
	ld a, 9
	ld [hl], a
	xor a
	ldh [rLCDC], a
	ldh [hScrollX], a
	ld hl, wOAMBuffer
	ld b, 160
.oamloop			; Remove all objects
	ldi [hl], a
	dec b
	jr nz, .oamloop
	ld hl, vBGMap0
	ld b, $FF
	ld c, 3
	ld a, " "
.tilemapLoop		; Clear the background
	ldi [hl], a
	dec b
	jr nz, .tilemapLoop
	ld b, $FF
	dec c
	jr nz, .tilemapLoop
	ld de, vBGMap0 + 4 * SCRN_VX_B + 11 ; $988B
	ld a, [wLives]
	ld b, a
	and a, $0F
	ld [de], a
	dec e
	ld a, b
	and a, $F0
	swap a
	ld [de], a		; Print lives at the appropriate position
	ld a, %10000011	
	ld [rLCDC], a	; Turn on LCD, background, objects
	ld a, STATE_ENTER_BONUS_GAME
	ldh [hGameState], a
	ret

HandleEnterBonusGame:: ; 3DD7
	xor a
	ldh [rLCDC], a
	ld hl, vBGMap0
	ld a, $F5		; Top Left corner
	ldi [hl], a
	ld b, SCRN_Y_B	; screen height
	ld a, $9F
.topLoop			; Top border
	ldi [hl], a
	dec b
	jr nz, .topLoop
	ld a, $FC
	ld [hl], a		; Top Right corner
	ld de, SCRN_VX_B ; screen width
	ld l, e
	ld b, $10
	ld c, 2
	ld a, $F8
.sideLoop			; Left and right border
	ld [hl], a
	add hl, de
	dec b
	jr nz, .sideLoop
	ld l, $33
	dec h
	dec h
	ld b, $10
	dec c
	jr nz, .sideLoop
	ld hl, vBGMap0 + 17 * SCRN_VX_B ; $9A20
	ld a, $FF
	ldi [hl], a		; Bottom Left corner
	ld b, $12
	ld a, $9F
.bottomLoop			; Bottom border
	ldi [hl], a
	dec b
	jr nz, .bottomLoop
	ld a, $E9
	ld [hl], a		; Bottom Right corner
	ld hl, vBGMap0 + 2 * SCRN_VX_B + 5 ; $9845
	ld a, "b"
	ldi [hl], a
	ld a, "o"
	ldi [hl], a
	dec a			; n
	ldi [hl], a
	ld a, "u"
	ldi [hl], a
	ld a, "s"
	ldi [hl], a
	inc l			; space
	ld a, "g"
	ldi [hl], a
	ld a, "a"
	ldi [hl], a
	ld a, "m"
	ldi [hl], a
	ld a, "e"
	ld [hl], a		; bonus game
	ld hl, vBGMap0 + 4 * SCRN_VX_B + 7 ; $9887
	ld a, $E4		; mario head
	ldi [hl], a
	inc l
	ld a, "*"
	ld [hl], a
	ld l, $E1
	ld a, $2D
	ld b, SCRN_Y_B		; screen height?
.topFloor
	ldi [hl], a
	dec b
	jr nz, .topFloor
	ld l, $D1
	ld a, "*"
	ldi [hl], a
	ld l, $41
	inc h
	ld a, $2D
	ld b, SCRN_Y_B		; screen height?
.highMidFloor
	ldi [hl], a
	dec b
	jr nz, .highMidFloor
	ld l, $31
	ld a, "*"
	ldi [hl], a
	ld l, $A1
	ld a, $2D
	ld b, SCRN_Y_B		; screen height?
.lowMidFloor
	ldi [hl], a
	dec b
	jr nz, .lowMidFloor
	ld l, $91
	ld a, "*"
	ldi [hl], a
	ld l, $01
	inc h
	ld a, $2D
	ld b, SCRN_Y_B		; screen height?
.bottomFloor
	ldi [hl], a
	dec b
	jr nz, .bottomFloor
	ld l, $F1
	dec h
	ld a, "*"
	ldi [hl], a
.prizePermutations					; $E5 is a flower
	db 0, 1, 2, $E5, 3, 1, 2, $E5	; These happen to be valid opcodes

	ld de, .prizePermutations		; with no side effects. Neat :) 
	ldh a, [rDIV]
	and a, 3
	inc a				; "Random" number from 1 to 4
.addAtoDE
	inc de
	dec a
	jr nz, .addAtoDE	; No ADD DE, A instruction
	ld hl, vBGMap0 + 6 * SCRN_VX_B + 18 ; $98D2		; First prize
	ld bc, SCRN_VX_B * 3			; 3 screen rows
.displayPrize
	ld a, [de]
	ld [hl], a
	inc de
	add hl, bc
	ld a, l
	cp a, $52
	jr nz, .displayPrize
	ld a, %10000011			; TODO make this a constant
	ldh [rLCDC], a
	ld a, STATE_BONUS_GAME_SETUP_MARIO
	ldh [hGameState], a
	ret

; draw the ladder
HandleMoveLadder:: ; 3EA7
	ld bc, SCRN_VX_B		; screen width
.drawLadder
	ld de, wLadderTiles
	ld a, [wLadderLocationHi]	; big endian
	cp $9A
	jr z, .jmp_3EE0
	ld h, a
	ld a, [wLadderLocationLo]
	ld l, a
.drawLadderLoop
	ld a, [de]
	ld [hl], a
	inc de
	add hl, bc
	push hl
	ld hl, $DA28		; loop counter?
	dec [hl]
	ld a, [hl]
	pop hl
	and a
	jr nz, .drawLadderLoop
	ld a, 4				; ladder has four segments
	ld [$DA28], a
	push hl
	ld hl, $DA29
	dec [hl]
	ld a, [hl]
	pop hl
	and a
	jr nz, .drawLadder	; why are we doing this 3 times?
	ld a, 3
	ld [$DA29], a
.nextState
	ld a, STATE_21
	ldh [hGameState], a
	ret

.jmp_3EE0				; TODO how could this ever get triggered :/
	xor a
	ld [$DA27], a		; ladder position in floors?
	jr .nextState

; Find the tile located at pixel coordinates FFAD and FFAE
; Return the memory location of the tile in FFAF and FFBO
; only called from LookupTile at $153
_LookupTile:: ; 3EE6
	ldh a, [$FFAD]
	sub a, $10			; Possibly to account for offset object coordinates
	srl a				; a coordinate of (8, 16) corresponds to the top-left
	srl a				; pixel.
	srl a				; Divide by 8, the number of pixels per tile
	ld de, $0000
	ld e, a
	ld hl, vBGMap0		; Start of background tile map
	ld b, SCRN_VX_B			; todo screen width
.loopY
	add hl, de
	dec b				; would make more sense to loop on A, or do some shifts
	jr nz, .loopY		; HL ← 9800 + A * screenWidth
	ldh a, [$FFAE]
	sub a, $08			; TODO why? Something to do with object coordinates
	srl a				; being offset?
	srl a
	srl a
	ld de, $0000
	ld e, a
	add hl, de
	ld a, h
	ldh [$FFB0], a
	ld a, l
	ldh [$FFAF], a
	ret

; called when mario hits a block that "moves" up and down
; FFB0 and FFAF now seem to contain the block above him?
; FFAD and FFAE now seem to determine where the block-sprite spawns...
Call_3F13::	; 3F13
	ldh a, [$FFB0]		; hey look at that. goes from 98 to ~9B
	ld d, a
	ldh a, [$FFAF]		; 00 to FF?
	ld e, a
	ld b, 4
.loop					; rr uses carry, used for a 16 bit shift by 4
	rr d
	rr e
	dec b
	jr nz, .loop
	ld a, e				; contains 2nd and 3rd nibble of the tile index
	sub a, $84			; 9840 is the top of the screen?
	and a, $FE			; ignore middle bit because mario is two tiles wide??
	rlca
	rlca
	add a, 8
	ldh [$FFAD], a		; override mario's y-coordinate?
	ldh a, [$FFAF]
	and a, $1F
	rla
	rla
	rla
	add a, 8
	ldh [$FFAE], a		; or maybe it's used for the block
	ret

; Display the score at wScore to the top right corner
; Print spaces instead of leading zeroes TODO Reuses FFB1?
DisplayScore:: ; 3F39
	ldh a, [$FFB1]	; Some check to see if the score needs to be  
	and a				; updated?
	ret z
	ld a, [$C0E2]
	and a
	ret nz
	ldh a, [$FFEA]
	cp a, 2
	ret z
	ld de, wScore + 2	; Start with the ten and hundred thousands
	ld hl, vBGMap0 + SCRN_VX_B ; $9820
.fromDEtoHL
	xor a
	ldh [$FFB1], a		; Start by printing spaces instead of leading zeroes
	ld c, 3 			; Maximum 3 digit pairs
.printDigitPair
	ld a, [de]
	ld b, a
	swap a				; Start with the more significant digit
	and a, $0F
	jr nz, .startNumber1
	ldh a, [$FFB1]	; If it's zero, check if the number has already started
	and a
	ld a, "0"
	jr nz, .printFirstDigit
	ld a, " "			; If not, start with spaces, not leading zeroes
.printFirstDigit
	ldi [hl], a			; Place the digit or space in VRAM
	ld a, b				; Now the lesser significant digit
	and a, $0F
	jr nz, .startNumber2; If non-zero, number has started (or already is)
	ldh a, [$FFB1]
	and a				; If zero, check if already started
	ld a, "0"
	jr nz, .printSecondDigit
	ld a, 1				; If the number still hasn't started at the ones,
	cp c				; score is 0. Print just one 0
	ld a, "0"
	jr z, .printSecondDigit
	ld a, " "			; Otherwise, print another space
.printSecondDigit
	ldi [hl], a			; Put the second digit in VRAM
	dec e				; Go to the next pair of digits in memory
	dec c				; Which is less significant
	jr nz, .printDigitPair
	xor a
	ldh [$FFB1], a
	ret

.startNumber1
	push af
	ld a, 1
	ldh [$FFB1], a	; Number has started, print "0" instead of " "
	pop af
	jr .printFirstDigit

.startNumber2
	push af
	ld a, 1
	ldh [$FFB1], a	; Number has started, print "0" instead of " "
	pop af
	jr .printSecondDigit

INCLUDE "oam_dma.asm"

HUDText::
db "mario*    world time"
db "       $*   1-1  000"

; TODO contains the flickering candle from world 1-3, the waves from 2-1
; maybe more animated sprites. 1BPP encoded?
; TODO put these in files
Data_3FC4::
	db $00, $00, $00, $10, $38, $38, $28, $10
	db $00, $E0, $B1, $5B, $FF, $FF, $FF, $FF
	db $7E, $3C, $18, $00, $00, $81, $42, $A5
	db $00, $E1, $33, $DE, $FF, $E7, $DB, $FF

	ds 26

	db $D3	; ....

	ds 1
