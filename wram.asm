SECTION "wram", WRAM0

wOAMBuffer::
	ds 4 * 3
wMarioOAMBuffer::
	ds 4 * 4

	ds 4 * 8
wDynamicOAMBuffer::
	ds 4 * 25

wScore::	; C0A0
	ds 3

wLivesEarnedLost::
	ds 1	; C0A3

; $38 if game over, zero otherwise
wIsGameOver::		; C0A4
	ds 1

wGameOverWindowEnabled :: ; C0A5
	db

wNumContinues::	; C0A6
	db

db ; C0A7

wContinueWorldAndLevel:: ; C0A8
	db

wSuperballLifeSpan:: ; C0A9
	db

ds $C0AD - $C0AA

wGameOverTimerExpired:: ; C0AD
	db

ds 2

wColumnBuffer:: ; C0B0
	ds $10

wTopScore:: ; C0C0
	ds 3

ds $C0D3 - $C0C3

wInvincibilityTimer:: ; C0D3
	db

wC0D4:: ; C0D4
	ds 1

wC0D5:: ; C0D5
	ds 1

wC0D6:: ; C0D6
	ds 1

wDemoTimer:: ; C0D7
	ds 1


ds $C0DD - $C0D8

wMarioDeathY:: ; C0DD
	ds 1

ds 1

wScrollY:: ; C0DF
	db

ds 1		; C0F0

wWinCount:: ; C0E1
	db

ds $C200 - $C0E2

wLevelData:: ; C200
wMarioVisible:: ; C200
	ds 1

wMarioPosY::  ; C201
	ds 1

wMarioPosX::  ; C202
	ds 1

wMarioAnimIndex:: ; C203
	ds 1

wC204:: ; C204
	ds 1

wMarioFacingDir:: ; C205
	ds 1

wC206:: ; C206
	ds 1

; Jump status
; 00 on ground;
; 01 ascending;
; 02 descending
wJumpStatus:: ; C207
	ds 1

wC208:: ; C208
	ds 1

wC209:: ; C209
	ds 1

wMarioOnGround:: ; C20A
	ds 1

wMarioAnimFrameCounter:: ; C20B
	ds 1

wMarioMomentum:: ; C20C
	ds 1

wMarioWalkingDir:: ; C20D
	ds 1

wMarioRunning:: ; C20E
	ds 1

wIsMarioMoving:: ; C20F
	ds 1

wEntityVisible:: ; C210
	ds 1

wEntityPosY:: ; C211
	ds 1

wEntityPosX:: ; C212
	ds 1

wEntityAnimIndex:: ; C213
	ds 1

ds $CF00 - $C214

wStack::	; CF00
	ds $100
wStackEnd:: ; $D000

ds 2

wCurrentCommand:: ; D002
	db

wCommandArgument:: ; D003
	db

ds $D013 - $D004

wObjectsDrawn:: ; D013 The upper 20 objects are used for enemies
	db

wBackgroundAnimated::	; D014
	db

; D100 - D190: enemies
ds $DA00 - $D015

wGameTimer:: ; DA00-DA02
	ds 3

wFloaty0_TTL:: ; DA03-DA06
	db
wFloaty1_TTL::
	db
wFloaty2_TTL::
	db
wFloaty3_TTL::
	db

wFloaty0_SpriteIfCoin:: ; DA07-DA0A
	db
wFloaty1_SpriteIfCoin::
	db
wFloaty2_SpriteIfCoin::
	db
wFloaty3_SpriteIfCoin::
	db

wNextFloatyOAMIndex :: ; DA0B
	ds 1

wFloaty0_IsCoin:: ; DA0C - DA0F
	db
wFloaty1_IsCoin::
	db
wFloaty2_IsCoin::
	db
wFloaty3_IsCoin::
	db

ds $DA15 - $DA10

wLives::	db	; $DA15

ds 1	; DA16
ds 1 	; DA17

wLadderLocationHi::	; DA18
	db

wLadderLocationLo:: ; DA19
	db

ds 1 ; DA1A

wBonusGameEndTimer:: ; DA1B
	db

ds 1				; DA1C

wGameTimerExpiringFlag:: ; DA1D do i have a better name?
	db

wBonusGameGrowAnimationFlag :: ; DA1E Long name...
	db

wBonusGameAnimationTimer:: ; DA1F
	db

ds $22 - $20

wBonusGameFrameCounter:: ; DA22
	db

wLadderTiles:: ; DA23
	ds 4

ds $DFE0 - $DA27

wPlaySquareSFX:: ; DFE0
	ds 1

wCurrentSquareSFX:: ; DFE1
	ds 1

ds 6

wPlaySong:: ; DFE8
	ds 1

wCurrentSong:: ; DFE9
	ds 1

ds $DFF0 - $DFEA

wPlayWaveSFX:: ; DFF0
	ds 1

wCurrentWaveSFX:: ; DFF1
	ds 1

ds $DFF8 - $DFF2

wPlayNoiseSFX:: ; DFF8
	ds 1

wCurrentNoiseSFX:: ; DFF9
	ds 1
