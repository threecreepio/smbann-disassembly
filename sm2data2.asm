;SMB2J DISASSEMBLY (SM2DATA2 portion)

;-------------------------------------------------------------------------------------
;DEFINES

FrameCounter          = $09
Enemy_State           = $1e
Enemy_Y_Position      = $cf
PiranhaPlantUpYPos    = $0417
PiranhaPlantDownYPos  = $0434
PiranhaPlant_Y_Speed  = $58
PiranhaPlant_MoveFlag = $a0

Player_X_Scroll       = $06ff

Player_PageLoc        = $6d
Player_X_Position     = $86

AreaObjectLength      = $0730
WindFlag              = $07f9
AreaType              = $074e

TimerControl          = $0747
EnemyFrameTimer       = $078a
WorldNumber           = $075f

Sprite_Y_Position     = $0200
Sprite_Tilenumber     = $0201
Sprite_Attributes     = $0202
Sprite_X_Position     = $0203

Alt_SprDataOffset     = $06ec

NoiseSoundQueue       = $fd

MetatileBuffer        = $06a1

; import from other files
.import GetPipeHeight
.import FindEmptyEnemySlot
.import SetupPiranhaPlant
.import VerticalPipeData
.import RenderUnderPart
; export to other files
.ifdef ANN
.export E_Area06
.export E_Area07
.export E_Area04
.export E_Area05
.export E_Area09
.export E_Area0B
.export E_Area1E
.export E_Area1F
.export E_Area12
.export E_Area21
.export E_Area15
.export E_Area16
.export E_Area18
.export E_Area19
.export E_Area1A
.export E_Area1B
.export E_Area22
.export E_Area27
.export E_Area28
.export E_Area2B
.export E_Area2A
.export L_Area06
.export L_Area07
.export L_Area04
.export L_Area05
.export L_Area09
.export L_Area0B
.export L_Area1E
.export L_Area1F
.export L_Area12
.export L_Area21
.export L_Area15
.export L_Area16
.export L_Area18
.export L_Area19
.export L_Area1A
.export L_Area1B
.export L_Area22
.export L_Area27
.export L_Area28
.export L_Area2B
.export L_Area2A
.export MRetainerCHRWorld5
.export MRetainerCHRWorld6
.export MRetainerCHRWorld7
.export HardWorldJumpSpringHandler
.export HardWorldEnemyGfxHandler
.else
.export E_CastleArea5
.export E_CastleArea6
.export E_CastleArea7
.export E_CastleArea8
.export E_GroundArea12
.export E_GroundArea13
.export E_GroundArea14
.export E_GroundArea15
.export E_GroundArea16
.export E_GroundArea17
.export E_GroundArea18
.export E_GroundArea19
.export E_GroundArea22
.export E_GroundArea23
.export E_GroundArea24
.export E_GroundArea29
.export E_UndergroundArea4
.export E_UndergroundArea5
.export E_WaterArea2
.export E_WaterArea4
.export E_WaterArea5
.export L_CastleArea5
.export L_CastleArea6
.export L_CastleArea7
.export L_CastleArea8
.export L_GroundArea12
.export L_GroundArea13
.export L_GroundArea14
.export L_GroundArea15
.export L_GroundArea16
.export L_GroundArea17
.export L_GroundArea18
.export L_GroundArea19
.export L_GroundArea22
.export L_GroundArea23
.export L_GroundArea24
.export L_GroundArea29
.export L_UndergroundArea4
.export L_UndergroundArea5
.export L_WaterArea2
.export L_WaterArea4
.export L_WaterArea5
.endif

;-------------------------------------------------------------------------------------------------
;$06 - used to store vertical length of pipe
;$07 - starts with adder from area parser, used to store row offset

UpsideDownPipe_High:
       lda #$01                     ;start at second row
       pha
       bne UDP
UpsideDownPipe_Low:
       lda #$04                     ;start at fifth row
       pha
UDP:   jsr GetPipeHeight            ;get pipe height from object byte
       pla
       sta $07                      ;save buffer offset temporarily
       tya
       pha                          ;save pipe height temporarily
       ldy AreaObjectLength,x       ;if on second column of pipe, skip this
       beq NoUDP
       jsr FindEmptyEnemySlot       ;otherwise try to insert upside-down
       bcs NoUDP                    ;piranha plant, if no empty slots, skip this
       lda #$04
       jsr SetupPiranhaPlant        ;set up upside-down piranha plant
       lda $06
       asl
       asl                          ;multiply height of pipe by 16
       asl                          ;and add enemy Y position previously set up
       asl                          ;then subtract 10 pixels, save as new Y position
       clc
       adc Enemy_Y_Position,x
       sec
       sbc #$0a
       sta Enemy_Y_Position,x
       sta PiranhaPlantDownYPos,x   ;set as "down" position
       clc                          ;add 24 pixels, save as "up" position
       adc #$18                     ;note up and down here are reversed
       sta PiranhaPlantUpYPos,x     
       inc PiranhaPlant_MoveFlag,x  ;set movement flag
NoUDP: pla
       tay                          ;return tile offset
       pha
       ldx $07
       lda VerticalPipeData+2,y
       ldy $06                      ;render the pipe shaft
       dey
       jsr RenderUnderPart
       pla
       tay
       lda VerticalPipeData,y       ;and render the pipe end
       sta MetatileBuffer,x
       rts

       rts                        ;unused, nothing jumps here

MoveUpsideDownPiranhaP:
      lda Enemy_State,x           ;check enemy state
      bne ExMoveUDPP              ;if set at all, branch to leave
      lda EnemyFrameTimer,x       ;check enemy's timer here
      bne ExMoveUDPP              ;branch to end if not yet expired
      lda PiranhaPlant_MoveFlag,x ;check movement flag
      bne SetupToMovePPlant       ;if moving, skip to part ahead
      lda PiranhaPlant_Y_Speed,x  ;get vertical speed
      eor #$ff
      clc                         ;change to two's compliment
      adc #$01
      sta PiranhaPlant_Y_Speed,x  ;save as new vertical speed
      inc PiranhaPlant_MoveFlag,x ;increment to set movement flag

SetupToMovePPlant:
      lda PiranhaPlantUpYPos,x    ;get original vertical coordinate (lowest point)
      ldy PiranhaPlant_Y_Speed,x  ;get vertical speed
      bpl RiseFallPiranhaPlant    ;branch if moving downwards
      lda PiranhaPlantDownYPos,x  ;otherwise get other vertical coordinate (highest point)

RiseFallPiranhaPlant:
       sta $00                     ;save vertical coordinate here
       lda TimerControl            ;get master timer control
       bne ExMoveUDPP              ;branch to leave if set (likely not necessary)
       lda Enemy_Y_Position,x      ;get current vertical coordinate
       clc
       adc PiranhaPlant_Y_Speed,x  ;add vertical speed to move up or down
       sta Enemy_Y_Position,x      ;save as new vertical coordinate
       cmp $00                     ;compare against low or high coordinate
       bne ExMoveUDPP              ;branch to leave if not yet reached
       lda #$00
       sta PiranhaPlant_MoveFlag,x ;otherwise clear movement flag
       lda #$20
       sta EnemyFrameTimer,x       ;set timer to delay piranha plant movement
ExMoveUDPP:
       rts

;-------------------------------------------------------------------------------------

.ifdef ANN
HardWorldJumpSpringHandler:
  ldy WorldNumber
  cpy #$01
  beq @Shift
  cpy #$02
  bne @Done
@Shift:
  lda #$E0
@Done:
  rts

HardWorldEnemyGfxHandler:
  ldy WorldNumber
  cpy #$01
  beq @Shift
  cpy #$02
  bne @Done
@Shift:
  lsr a
@Done:
  rts

;unused bytes
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF
.else
BlowPlayerAround:
        lda WindFlag            ;if wind is turned off, just exit
        beq ExBlow
        lda AreaType            ;don't blow the player around unless
        cmp #$01                ;the area is ground type
        bne ExBlow
        ldy #$01
        lda FrameCounter        ;branch to set d0 if on an odd frame
        asl
        bcs BThr                ;otherwise wind will only blow
        ldy #$03                ;one out of every four frames
BThr:   sty $00
        lda FrameCounter        ;throttle wind blowing by using the frame counter
        and $00                 ;to mask out certain frames
        bne ExBlow
        lda Player_X_Position   ;move player slightly to the right
        clc                     ;to simulate the wind moving the player
        adc #$01
        sta Player_X_Position
        lda Player_PageLoc
        adc #$00
        sta Player_PageLoc
        inc Player_X_Scroll     ;add one to movement speed for scroll
ExBlow: rts

;note the position data values are overwritten in RAM
LeavesYPos:
        .byte $30, $70, $b8, $50, $98, $30
        .byte $70, $b8, $50, $98, $30, $70

LeavesXPos:
        .byte $30, $30, $30, $60, $60, $a0
        .byte $a0, $a0, $d0, $d0, $d0, $60

LeavesTile:
        .byte $7b, $7b, $7b, $7b, $7a, $7a
        .byte $7b, $7b, $7b, $7a, $7b, $7a

SimulateWind:
          lda WindFlag             ;if no wind, branch to leave
          beq ExSimW
          lda #$04                 ;play wind sfx
          sta NoiseSoundQueue
          jsr ModifyLeavesPos      ;modify X and Y position data of leaves
          ldx #$00                 ;use mostly unused sprite data offset
          ldy Alt_SprDataOffset-1  ;for first six leaves
DrawLeaf: lda LeavesYPos,x
          sta Sprite_Y_Position,y  ;set up sprite data in OAM memory
          lda LeavesTile,x
          sta Sprite_Tilenumber,y
          lda #$41
          sta Sprite_Attributes,y
          lda LeavesXPos,x
          sta Sprite_X_Position,y
          iny
          iny
          iny
          iny
          inx                      ;if still on first six leaves, continue
          cpx #$06                 ;using the first sprite data offset
          bne DLLoop               ;otherwise use the next one instead
          ldy Alt_SprDataOffset    ;note the next one is also used by blocks
DLLoop:   cpx #$0c                 ;continue until done putting all leaves on the screen
          bne DrawLeaf
ExSimW:   rts

LeavesPosAdder:
   .byte $57, $57, $56, $56, $58, $58, $56, $56, $57, $58, $57, $58
   .byte $59, $59, $58, $58, $5a, $5a, $58, $58, $59, $5a, $59, $5a

ModifyLeavesPos:
         ldx #$0b
MLPLoop: lda LeavesXPos,x      ;add each adder to each X position twice
         clc                   ;and to each Y position once
         adc LeavesPosAdder,x
         adc LeavesPosAdder,x
         sta LeavesXPos,x
         lda LeavesYPos,x
         clc
         adc LeavesPosAdder,x
         sta LeavesYPos,x
         dex
         bpl MLPLoop
         rts

WindOn:
     lda #$01         ;branch to turn the wind on
     bne WOn
WindOff:
     lda #$00         ;turn the wind off
WOn: sta WindFlag
     rts

;some unused bytes
   .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff

;-------------------------------------------------------------------------------------
.endif

.ifdef ANN

E_Area06:
.byte $49,$9F,$67,$03,$79,$9D,$A0,$3A,$57,$9F,$BB,$1D,$D5,$25,$0F,$05
.byte $18,$1D,$74,$00,$84,$00,$94,$00,$C6,$29,$49,$9D,$DB,$05,$0F,$08
.byte $05,$1B,$09,$1D,$B0,$38,$80,$95,$C0,$3C,$EC,$A8,$CC,$8C,$4A,$9B
.byte $78,$2D,$90,$B5,$FF

E_Area07:
.byte $74,$80,$F0,$38,$A0,$BB,$40,$BC,$8C,$1D,$C9,$9D,$05,$9B,$1C,$0C
.byte $59,$1B,$B5,$1D,$2C,$8C,$40,$15,$7C,$1B,$DC,$1D,$6C,$8C,$BC,$0C
.byte $78,$AD,$A5,$28,$90,$B5,$FF

E_Area04:
.byte $27,$A9,$4B,$0C,$68,$29,$0F,$06,$77,$1B,$0F,$0B,$60,$15,$4B,$8C
.byte $78,$2D,$90,$B5,$FF

E_Area05:
.byte $19,$9B,$99,$1B,$2C,$8C,$59,$1B,$C5,$0F,$0E,$82,$E0,$0F,$06,$2E
.byte $65,$E7,$0F,$08,$9B,$07,$0E,$82,$E0,$39,$0E,$87,$10,$BD,$28,$59
.byte $9F,$0F,$0F,$34,$0F,$77,$10,$9E,$65,$F1,$0F,$12,$0E,$65,$E3,$78
.byte $2D,$0F,$15,$3B,$29,$57,$82,$0F,$18,$55,$1D,$78,$2D,$90,$B5,$FF

E_Area09:
.byte $EB,$8E,$0F,$03,$FB,$05,$17,$85,$DB,$8E,$0F,$07,$57,$05,$7B,$05
.byte $9B,$80,$2B,$85,$FB,$05,$0F,$0B,$1B,$05,$9B,$05,$FF

E_Area0B:
.byte $0E,$C3,$A6,$AB,$00,$BB,$8E,$6B,$82,$DE,$00,$A0,$33,$86,$43,$06
.byte $3E,$BA,$A0,$CB,$02,$0F,$07,$7E,$43,$A4,$83,$02,$0F,$0A,$3B,$02
.byte $CB,$37,$0F,$0C,$E3,$0E,$FF

E_Area1E:
.byte $E6,$A9,$57,$A8,$B5,$24,$19,$A4,$76,$28,$A2,$0F,$95,$8F,$9D,$A8
.byte $0F,$07,$09,$29,$55,$24,$8B,$17,$A9,$24,$DB,$83,$04,$A9,$24,$8F
.byte $65,$0F,$FF

E_Area1F:
.byte $0F,$02,$28,$10,$E6,$03,$D8,$90,$0F,$05,$85,$0F,$78,$83,$C8,$10
.byte $18,$83,$58,$83,$F7,$90,$0F,$0C,$43,$0F,$73,$8F,$FF

E_Area12:
.byte $0B,$80,$60,$38,$10,$B8,$C0,$3B,$DB,$8E,$40,$B8,$F0,$38,$7B,$8E
.byte $A0,$B8,$C0,$B8,$FB,$00,$A0,$B8,$30,$BB,$EE,$43,$86,$0F,$0B,$2B
.byte $0E,$67,$0E,$FF

E_Area21:
.byte $0A,$AA,$0E,$31,$88,$FF

E_Area15:
.byte $CD,$A5,$B5,$A8,$07,$A8,$76,$28,$CC,$25,$65,$A4,$A9,$24,$E5,$24
.byte $19,$A4,$64,$8F,$95,$A8,$E6,$24,$19,$A4,$D7,$29,$16,$A9,$58,$29
.byte $97,$29,$FF

E_Area16:
.byte $0F,$02,$02,$11,$0F,$07,$02,$11,$FF

E_Area18:
.byte $2B,$82,$AB,$38,$DE,$43,$E2,$1B,$B8,$EB,$3B,$DB,$80,$8B,$B8,$1B
.byte $82,$FB,$B8,$7B,$80,$FB,$3C,$5B,$BC,$7B,$B8,$1B,$8E,$CB,$0E,$1B
.byte $8E,$0F,$0D,$2B,$3B,$BB,$B8,$EB,$82,$4B,$B8,$BB,$38,$3B,$B7,$BB
.byte $02,$0F,$13,$1B,$00,$CB,$80,$6B,$BC,$FF

E_Area19:
.byte $7B,$80,$AE,$00,$80,$8B,$8E,$E8,$05,$F9,$86,$17,$86,$16,$85,$4E
.byte $39,$80,$AB,$8E,$87,$85,$C3,$05,$8B,$82,$9B,$02,$AB,$02,$BB,$86
.byte $CB,$06,$D3,$03,$3B,$8E,$6B,$0E,$A7,$8E,$FF

E_Area1A:
.byte $29,$8E,$52,$11,$83,$0E,$0F,$03,$3B,$0E,$9B,$0E,$2B,$8E,$5B,$0E
.byte $CB,$8E,$FB,$0E,$FB,$82,$9B,$82,$BB,$02,$FE,$43,$E6,$BB,$8E,$0F
.byte $0A,$AB,$0E,$CB,$0E,$F9,$0E,$88,$86,$A6,$06,$DB,$02,$B6,$8E,$FF

E_Area1B:
.byte $AB,$CE,$DE,$43,$C0,$CB,$CE,$5B,$8E,$1B,$CE,$4B,$85,$67,$45,$0F
.byte $07,$2B,$00,$7B,$85,$97,$05,$0F,$0A,$92,$02,$FF

E_Area22:
.byte $0A,$AA,$1E,$23,$AA,$FF

E_Area27:
.byte $1E,$B3,$C7,$0F,$03,$1E,$30,$E7,$0F,$05,$1E,$23,$AB,$0F,$07,$1E
.byte $2A,$8A,$2E,$23,$A2,$2E,$32,$EA,$FF

E_Area28:
.byte $3B,$87,$66,$27,$CC,$27,$EE,$31,$87,$EE,$23,$A7,$3B,$87,$DB,$07
.byte $FF

E_Area2B:
.byte $2E,$B8,$C1,$5B,$07,$AB,$07,$69,$87,$BA,$07,$FB,$87,$65,$A7,$6A
.byte $27,$A6,$A7,$AC,$27,$1B,$87,$88,$07,$2B,$83,$7B,$07,$A7,$90,$E5
.byte $83,$14,$A7,$19,$27,$77,$07,$F8,$07,$47,$8F,$B9,$07,$FF

E_Area2A:
.byte $07,$9B,$0A,$07,$B9,$1B,$66,$9B,$78,$07,$AE,$65,$E5,$FF

L_Area06:
.byte $9B,$07,$05,$32,$06,$33,$07,$34,$4E,$03,$5C,$02,$0C,$F1,$27,$00
.byte $3C,$74,$47,$0B,$FC,$00,$FE,$0B,$77,$8B,$EE,$09,$FE,$0A,$45,$B2
.byte $55,$0B,$99,$32,$B9,$0B,$FE,$02,$0E,$85,$FE,$02,$16,$8B,$2E,$0C
.byte $AE,$0A,$EE,$05,$1E,$82,$47,$0B,$07,$BD,$C4,$72,$DE,$0A,$FE,$02
.byte $03,$8B,$07,$0B,$13,$3C,$17,$3D,$E3,$02,$EE,$0A,$F3,$04,$F7,$02
.byte $FE,$0E,$FE,$8A,$38,$E4,$4A,$72,$68,$64,$37,$B0,$98,$64,$A8,$64
.byte $E8,$64,$F8,$64,$0D,$C4,$71,$64,$CD,$43,$CE,$09,$DD,$42,$DE,$0B
.byte $FE,$02,$5D,$C7,$FD

L_Area07:
.byte $9B,$87,$05,$32,$06,$33,$07,$34,$03,$E2,$0E,$06,$1E,$0C,$7E,$0A
.byte $8E,$05,$8E,$82,$8A,$8B,$8E,$0A,$EE,$02,$0A,$E0,$19,$61,$23,$04
.byte $28,$62,$2E,$0B,$7E,$0A,$81,$62,$87,$30,$8E,$04,$A7,$31,$C7,$0B
.byte $D7,$33,$FE,$03,$03,$8B,$0E,$0A,$11,$62,$1E,$04,$27,$32,$4E,$0A
.byte $51,$62,$57,$0B,$5E,$04,$67,$34,$9E,$0A,$A1,$62,$AE,$03,$B3,$0B
.byte $BE,$0B,$EE,$09,$FE,$0A,$2E,$82,$7A,$0B,$7E,$0A,$97,$31,$A6,$10
.byte $BE,$04,$DA,$0B,$EE,$0A,$F1,$62,$FE,$02,$3E,$8A,$7E,$06,$AE,$0A
.byte $CE,$06,$FE,$0A,$0D,$C4,$11,$53,$21,$52,$24,$08,$51,$52,$61,$52
.byte $CD,$43,$CE,$09,$DD,$42,$DE,$0B,$FE,$02,$5D,$C7,$FD

L_Area04:
.byte $5B,$07,$05,$32,$06,$33,$07,$34,$FE,$0A,$AE,$86,$BE,$07,$FE,$02
.byte $0D,$02,$27,$32,$46,$61,$55,$62,$5E,$0E,$1E,$82,$68,$3C,$74,$3A
.byte $7D,$4B,$5E,$8E,$7D,$4B,$7E,$82,$84,$62,$94,$61,$A4,$31,$BD,$4B
.byte $CE,$06,$FE,$02,$0D,$06,$34,$31,$3E,$0A,$64,$32,$75,$0B,$7B,$61
.byte $A4,$33,$AE,$02,$DE,$0E,$3E,$82,$64,$32,$78,$32,$B4,$36,$C8,$36
.byte $DD,$4B,$44,$B2,$58,$32,$94,$63,$A4,$3E,$BA,$30,$C9,$61,$CE,$06
.byte $DD,$4B,$CE,$86,$DD,$4B,$FE,$0A,$2E,$86,$5E,$0A,$7E,$06,$FE,$02
.byte $1E,$86,$3E,$0A,$5E,$06,$7E,$02,$9E,$06,$FE,$0A,$0D,$C4,$CD,$43
.byte $CE,$09,$DE,$0B,$DD,$42,$FE,$02,$5D,$C7,$FD

L_Area05:
.byte $5B,$03,$05,$34,$06,$35,$07,$36,$6E,$0A,$EE,$02,$FE,$05,$0D,$01
.byte $17,$0B,$97,$0B,$9E,$02,$C6,$04,$FA,$30,$FE,$0A,$4E,$82,$57,$0B
.byte $58,$62,$68,$62,$79,$61,$8A,$60,$8E,$0A,$F5,$31,$F9,$7B,$39,$F3
.byte $97,$33,$B5,$71,$39,$F3,$4D,$48,$9E,$02,$AE,$05,$CD,$4A,$ED,$4B
.byte $0E,$81,$17,$04,$39,$73,$5C,$02,$85,$65,$95,$32,$A9,$7B,$CC,$03
.byte $5E,$8F,$6D,$47,$FE,$02,$0D,$07,$39,$73,$4E,$0A,$AE,$02,$E7,$23
.byte $07,$88,$39,$73,$E6,$04,$39,$FB,$4E,$0A,$C4,$31,$EB,$61,$FE,$02
.byte $07,$B0,$1E,$0A,$4E,$06,$57,$0B,$BE,$02,$C9,$61,$DA,$60,$ED,$4B
.byte $0E,$85,$0D,$0E,$FE,$0A,$78,$E4,$8E,$06,$BF,$47,$EE,$0F,$6D,$C7
.byte $0E,$82,$39,$73,$9A,$60,$A9,$61,$AE,$06,$DE,$0A,$E7,$02,$EB,$79
.byte $F7,$02,$FE,$06,$0D,$14,$FE,$0A,$5E,$82,$78,$74,$9E,$0A,$F8,$64
.byte $FE,$0B,$9E,$84,$BE,$05,$BE,$82,$DA,$60,$E9,$61,$F8,$62,$FE,$0A
.byte $0D,$C4,$11,$64,$51,$62,$CD,$43,$CE,$09,$DD,$42,$DE,$0B,$FE,$02
.byte $5D,$C7,$FD

L_Area09:
.byte $90,$B1,$0F,$26,$29,$91,$7E,$42,$FE,$40,$28,$92,$4E,$42,$2E,$C0
.byte $57,$73,$C3,$27,$C7,$27,$D3,$05,$5C,$81,$77,$63,$88,$62,$99,$61
.byte $AA,$60,$BC,$01,$EE,$42,$4E,$C0,$69,$11,$7E,$42,$DE,$40,$F8,$62
.byte $0E,$C2,$AE,$40,$D7,$63,$E7,$63,$33,$A5,$37,$27,$82,$42,$93,$05
.byte $A3,$20,$CC,$01,$E7,$73,$0C,$81,$3E,$42,$0D,$0A,$5E,$40,$88,$72
.byte $BE,$42,$E7,$88,$FE,$40,$39,$E1,$4E,$00,$69,$60,$87,$60,$A5,$60
.byte $C3,$31,$FE,$31,$6D,$C1,$BE,$42,$EF,$20,$8D,$C7,$FD

L_Area0B:
.byte $54,$21,$0F,$26,$A7,$22,$37,$FB,$73,$05,$83,$08,$87,$02,$93,$20
.byte $C7,$73,$04,$F1,$06,$31,$39,$71,$59,$71,$E7,$73,$37,$A0,$47,$08
.byte $86,$7C,$E5,$71,$E7,$31,$33,$A4,$39,$71,$A9,$71,$D3,$23,$08,$F2
.byte $13,$06,$27,$02,$49,$71,$75,$75,$E8,$72,$67,$F3,$99,$71,$E7,$20
.byte $F4,$72,$F7,$31,$17,$A0,$33,$20,$39,$71,$73,$28,$BC,$05,$39,$F1
.byte $79,$71,$A6,$21,$C3,$21,$DC,$00,$FC,$00,$07,$A2,$13,$20,$23,$07
.byte $5F,$32,$8C,$00,$98,$7A,$C7,$63,$D9,$61,$03,$A2,$07,$22,$74,$72
.byte $77,$31,$E7,$73,$39,$F1,$58,$72,$77,$73,$D8,$72,$7F,$B1,$97,$73
.byte $B6,$64,$C5,$65,$D4,$66,$E3,$67,$F3,$67,$8D,$C1,$CF,$26,$AD,$C7
.byte $FD

L_Area1E:
.byte $50,$11,$0F,$26,$FE,$10,$8B,$93,$A9,$0C,$14,$C1,$CC,$16,$CF,$11
.byte $2F,$95,$B7,$14,$C7,$96,$D6,$44,$2B,$92,$39,$0C,$72,$41,$A7,$00
.byte $1B,$95,$97,$13,$6C,$95,$6F,$11,$A2,$40,$BF,$15,$C2,$40,$0B,$9F
.byte $53,$16,$62,$44,$72,$C2,$9B,$1D,$B7,$E0,$ED,$4A,$03,$E0,$8E,$11
.byte $9D,$41,$BE,$42,$EF,$20,$CD,$C7,$FD

L_Area1F:
.byte $50,$11,$0F,$26,$AF,$32,$D8,$62,$DE,$10,$08,$E4,$5A,$62,$6C,$4C
.byte $86,$43,$AD,$48,$3A,$E2,$53,$42,$88,$64,$9C,$36,$08,$E4,$4A,$62
.byte $5C,$4D,$3A,$E2,$9C,$32,$FC,$41,$3C,$B1,$83,$00,$AC,$42,$2A,$E2
.byte $3C,$46,$AA,$62,$BC,$4E,$C6,$43,$46,$C3,$AA,$62,$BD,$48,$0B,$96
.byte $47,$05,$C7,$12,$3C,$C2,$9C,$41,$CD,$48,$DC,$32,$4C,$C2,$BC,$32
.byte $1C,$B1,$5A,$62,$6C,$44,$76,$43,$BA,$62,$DC,$32,$5D,$CA,$73,$12
.byte $E3,$12,$8E,$91,$9D,$41,$BE,$42,$EF,$20,$CD,$C7,$FD

L_Area12:
.byte $95,$B1,$0F,$26,$0D,$02,$C8,$72,$1C,$81,$38,$72,$0D,$05,$97,$34
.byte $98,$62,$A3,$20,$B3,$07,$C3,$20,$CC,$03,$F9,$91,$2C,$81,$48,$62
.byte $0D,$09,$37,$63,$47,$03,$57,$02,$8C,$02,$C5,$79,$C7,$31,$F9,$11
.byte $39,$F1,$A9,$11,$6F,$B4,$D3,$65,$E3,$65,$7D,$C1,$BF,$26,$9D,$C7
.byte $FD

L_Area21:
.byte $00,$C1,$4C,$00,$F4,$4F,$0D,$02,$02,$42,$43,$4F,$52,$C2,$DE,$00
.byte $5A,$C2,$4D,$C7,$FD

L_Area15:
.byte $97,$11,$0F,$26,$FE,$10,$2B,$92,$57,$12,$8B,$12,$C0,$41,$F7,$13
.byte $5B,$92,$69,$0C,$BB,$12,$B2,$46,$19,$93,$71,$00,$17,$94,$7C,$14
.byte $7F,$11,$93,$41,$BF,$15,$FC,$13,$FF,$11,$2F,$95,$50,$42,$51,$12
.byte $58,$14,$A6,$12,$DB,$12,$1B,$93,$46,$43,$7B,$12,$8D,$49,$B7,$14
.byte $1B,$94,$49,$0C,$BB,$12,$FC,$13,$FF,$12,$03,$C1,$2F,$15,$43,$12
.byte $4B,$13,$77,$13,$9D,$4A,$15,$C1,$A1,$41,$C3,$12,$FE,$01,$7D,$C1
.byte $9E,$42,$CF,$20,$9D,$C7,$FD

L_Area16:
.byte $52,$21,$0F,$20,$6E,$44,$0C,$F1,$4C,$01,$AA,$35,$D9,$34,$EE,$20
.byte $08,$B3,$37,$32,$43,$08,$4E,$21,$53,$20,$7C,$01,$97,$21,$B7,$05
.byte $9C,$81,$E7,$42,$5F,$B3,$97,$63,$AC,$02,$C5,$41,$49,$E0,$58,$61
.byte $76,$64,$85,$65,$94,$66,$A4,$22,$A6,$03,$C8,$22,$DC,$02,$68,$F2
.byte $96,$42,$13,$82,$17,$02,$AF,$34,$F6,$21,$FC,$06,$26,$80,$2A,$24
.byte $36,$01,$8C,$00,$FF,$35,$4E,$A0,$55,$21,$77,$20,$87,$08,$89,$22
.byte $AE,$21,$4C,$82,$9F,$34,$EC,$01,$03,$E7,$13,$67,$8D,$4A,$AD,$41
.byte $0F,$A6,$CD,$47,$FD

L_Area18:
.byte $92,$31,$0F,$20,$6E,$40,$0D,$02,$37,$73,$EC,$00,$0C,$80,$3C,$00
.byte $6C,$00,$9C,$00,$06,$C0,$C7,$73,$06,$84,$28,$72,$96,$40,$E7,$73
.byte $26,$C0,$87,$7B,$D2,$41,$39,$F1,$C8,$F2,$97,$E3,$A3,$23,$E7,$02
.byte $E3,$08,$F3,$22,$37,$E3,$9C,$00,$BC,$00,$EC,$00,$0C,$80,$3C,$00
.byte $86,$27,$5C,$80,$7C,$00,$9C,$00,$29,$E1,$DC,$05,$F6,$41,$DC,$80
.byte $E8,$72,$0C,$81,$27,$73,$4C,$01,$66,$74,$A6,$07,$0D,$11,$3F,$35
.byte $B6,$41,$2C,$82,$36,$40,$7C,$02,$86,$40,$F9,$61,$16,$83,$39,$61
.byte $AC,$04,$C6,$41,$0C,$83,$16,$41,$88,$F2,$39,$F1,$7C,$00,$89,$61
.byte $9C,$00,$A7,$63,$BC,$00,$C5,$65,$DC,$00,$E3,$67,$F3,$67,$8D,$C1
.byte $CF,$26,$AD,$C7,$FD

L_Area19:
.byte $55,$B1,$0F,$26,$CF,$33,$07,$B2,$15,$11,$52,$42,$99,$0C,$AC,$02
.byte $D3,$24,$D6,$42,$D7,$25,$23,$85,$CF,$33,$07,$E3,$19,$61,$78,$7A
.byte $EF,$33,$2C,$81,$46,$64,$55,$65,$65,$65,$0C,$F4,$53,$05,$62,$41
.byte $63,$21,$96,$22,$9A,$41,$CC,$03,$B9,$91,$C3,$06,$E6,$02,$39,$F1
.byte $63,$26,$67,$27,$D3,$07,$FC,$01,$18,$E2,$D9,$08,$E9,$05,$0C,$86
.byte $37,$22,$93,$24,$87,$85,$AC,$02,$C2,$41,$C3,$23,$D9,$71,$FC,$01
.byte $7F,$B1,$9C,$00,$A7,$63,$B6,$64,$CC,$00,$D4,$66,$E3,$67,$F3,$67
.byte $8D,$C1,$CF,$26,$AD,$C7,$FD

L_Area1A:
.byte $50,$B1,$0F,$26,$FC,$00,$1F,$B3,$5C,$00,$65,$65,$74,$66,$83,$67
.byte $93,$67,$DC,$73,$4C,$80,$B3,$20,$C3,$09,$C9,$0C,$D3,$2F,$DC,$00
.byte $2C,$80,$4C,$00,$8C,$00,$D3,$2E,$ED,$4A,$FC,$00,$93,$85,$97,$02
.byte $EC,$01,$4C,$80,$59,$11,$D8,$11,$DA,$10,$37,$A0,$47,$08,$99,$11
.byte $E7,$21,$3A,$90,$67,$20,$76,$10,$77,$60,$87,$20,$D8,$12,$39,$F1
.byte $AC,$00,$E9,$71,$0C,$80,$2C,$00,$4C,$05,$C7,$7B,$39,$F1,$EC,$00
.byte $F9,$11,$0C,$82,$6F,$34,$F8,$11,$FA,$10,$7F,$B2,$AC,$00,$B6,$64
.byte $CC,$01,$E3,$67,$F3,$67,$8D,$C1,$CF,$26,$AD,$C7,$FD

L_Area1B:
.byte $52,$B1,$0F,$20,$6E,$45,$39,$91,$B3,$08,$C3,$21,$C8,$11,$CA,$10
.byte $49,$91,$7C,$71,$97,$00,$A7,$01,$E8,$12,$88,$91,$8A,$10,$E7,$20
.byte $F7,$08,$05,$91,$07,$30,$17,$21,$49,$11,$9C,$01,$C8,$72,$2C,$E6
.byte $2C,$76,$D3,$03,$D8,$7A,$89,$91,$D8,$72,$39,$F1,$A9,$11,$09,$F1
.byte $33,$26,$37,$27,$A3,$08,$D8,$62,$28,$91,$2A,$10,$56,$21,$70,$05
.byte $79,$0C,$8C,$00,$94,$21,$9F,$35,$2F,$B8,$3D,$C1,$7F,$26,$5D,$C7
.byte $FD

L_Area22:
.byte $06,$C1,$4C,$00,$F4,$4F,$0D,$02,$06,$20,$24,$4F,$35,$A0,$36,$20
.byte $53,$46,$D5,$20,$D6,$20,$34,$A1,$73,$49,$74,$20,$94,$20,$B4,$20
.byte $D4,$20,$F4,$20,$2E,$80,$59,$42,$4D,$C7,$FD

L_Area27:
.byte $48,$01,$0E,$01,$00,$5A,$3E,$06,$45,$46,$47,$46,$53,$44,$AE,$01
.byte $DF,$4A,$4D,$C7,$0E,$81,$00,$5A,$2E,$04,$37,$28,$3A,$48,$46,$47
.byte $C7,$08,$CE,$0F,$DF,$4A,$4D,$C7,$0E,$81,$00,$5A,$2E,$02,$36,$47
.byte $37,$52,$3A,$49,$47,$25,$A7,$52,$D7,$05,$DF,$4A,$4D,$C7,$0E,$81
.byte $00,$5A,$3E,$02,$44,$51,$53,$44,$54,$44,$55,$24,$A1,$54,$AE,$01
.byte $B4,$21,$DF,$4A,$E5,$08,$4D,$C7,$FD

L_Area28:
.byte $41,$01,$B4,$34,$C8,$52,$F2,$51,$47,$D3,$6C,$03,$65,$49,$9E,$07
.byte $BE,$01,$CC,$03,$FE,$07,$0D,$C9,$1E,$01,$6C,$01,$62,$35,$63,$53
.byte $8A,$41,$AC,$01,$B3,$53,$E9,$51,$26,$C3,$27,$33,$63,$43,$64,$33
.byte $BA,$60,$C9,$61,$CE,$0B,$D4,$31,$E5,$0A,$EE,$0F,$7D,$CA,$7D,$47
.byte $FD

L_Area2B:
.byte $41,$01,$27,$D3,$79,$51,$C4,$56,$00,$E2,$03,$53,$0C,$0F,$12,$3B
.byte $1A,$42,$43,$54,$6D,$49,$83,$53,$99,$53,$C3,$54,$DA,$52,$0C,$84
.byte $09,$53,$53,$64,$63,$31,$67,$34,$86,$41,$8C,$01,$A3,$30,$B3,$64
.byte $CC,$03,$D9,$42,$5C,$84,$A0,$62,$A8,$62,$B0,$62,$B8,$62,$C0,$62
.byte $C8,$62,$D0,$62,$D8,$62,$E0,$62,$E8,$62,$16,$C2,$58,$52,$8C,$04
.byte $A7,$55,$D0,$63,$D7,$65,$E2,$61,$E7,$65,$F2,$61,$F7,$65,$13,$B8
.byte $17,$38,$8C,$03,$1D,$C9,$50,$62,$5C,$0B,$62,$3E,$63,$52,$8A,$52
.byte $93,$54,$AA,$42,$D3,$51,$EA,$41,$03,$D3,$1C,$04,$1A,$52,$33,$55
.byte $73,$44,$77,$44,$16,$D2,$19,$31,$1A,$32,$5C,$0F,$9A,$47,$95,$64
.byte $A5,$64,$B5,$64,$C5,$64,$D5,$64,$E5,$64,$F5,$64,$05,$E4,$40,$61
.byte $42,$35,$56,$34,$5C,$09,$A2,$61,$A6,$61,$B3,$34,$B7,$34,$FC,$08
.byte $0C,$87,$28,$54,$59,$53,$9A,$30,$A9,$61,$B8,$62,$BE,$0B,$C4,$31
.byte $D5,$0A,$DE,$0F,$0D,$CA,$7D,$47,$FD

L_Area2A:
.byte $07,$0F,$0E,$02,$39,$73,$05,$8B,$2E,$0B,$B7,$0B,$64,$8B,$6E,$02
.byte $CE,$06,$DE,$0F,$E6,$0A,$7D,$C7,$FD

MRetainerCHRWorld5:
.byte $0F,$3F,$7F,$7F,$F3,$ED,$FF,$FD
.byte $0F,$3F,$78,$60,$C0,$C0,$80,$00
.byte $FF,$FB,$7C,$FF,$70,$7F,$FF,$FF
.byte $00,$00,$00,$80,$43,$40,$E0,$F8
.byte $FC,$FE,$7F,$10,$00,$00,$00,$FC
.byte $FF,$FF,$63,$0F,$3F,$7C,$78,$FC
.byte $F0,$FC,$FE,$FE,$CF,$B7,$FF,$BF
.byte $F0,$FC,$1E,$06,$03,$03,$01,$00
.byte $FF,$DF,$3E,$FF,$0E,$FE,$FF,$FF
.byte $00,$00,$00,$01,$42,$02,$07,$1F
.byte $3F,$7F,$FE,$08,$00,$00,$00,$3F
.byte $FF,$FF,$C6,$F0,$FC,$3E,$1E,$3F

MRetainerCHRWorld6:
.byte $03,$07,$1F,$7F,$7F,$FF,$FD,$FF
.byte $03,$07,$1F,$7F,$7F,$FF,$FC,$F8
.byte $71,$39,$0F,$0F,$1F,$1F,$1E,$3F
.byte $4A,$03,$01,$01,$11,$10,$18,$39
.byte $3B,$3F,$3C,$3C,$18,$00,$00,$3F
.byte $24,$03,$03,$02,$07,$07,$07,$3F
.byte $C0,$F0,$F0,$FC,$FE,$FF,$8F,$FF
.byte $C0,$F0,$F0,$FC,$9E,$17,$07,$01
.byte $8E,$9E,$F8,$F8,$FE,$FE,$3F,$F7
.byte $50,$40,$00,$00,$86,$07,$0F,$CF
.byte $F7,$F7,$02,$00,$1C,$3E,$3E,$1C
.byte $1B,$FB,$FC,$DC,$FC,$3E,$3E,$1C

MRetainerCHRWorld7:
.byte $1F,$7F,$70,$C1,$C1,$95,$A3,$CB
.byte $1F,$7F,$7F,$FE,$FE,$EA,$C4,$08
.byte $F3,$FD,$FC,$3F,$38,$1C,$1F,$17
.byte $00,$02,$03,$00,$00,$63,$70,$F8
.byte $10,$38,$7F,$7F,$3F,$7F,$78,$00
.byte $FF,$DF,$9F,$3B,$3F,$7F,$78,$F8
.byte $F0,$FC,$1C,$06,$06,$82,$82,$46
.byte $F0,$FC,$FC,$FE,$FE,$7E,$1E,$48
.byte $3E,$FE,$7E,$F8,$38,$70,$E0,$D6
.byte $00,$00,$80,$00,$00,$88,$1C,$38
.byte $1F,$3F,$FF,$FE,$F8,$FC,$3E,$00
.byte $F0,$F0,$F0,$B8,$F8,$FC,$3E,$3F

;unused byte
.byte $FF

.else
;level 5-4
E_CastleArea5:
  .byte $2a, $a9, $6b, $0c, $cb, $0c, $15, $9c, $89, $1c, $cc, $1d, $09, $9d, $f5, $1c
  .byte $6b, $a9, $ab, $0c, $db, $29, $48, $9d, $9b, $0c, $5b, $8c, $a5, $1c, $49, $9d
  .byte $79, $1d, $09, $9d, $6b, $0c, $c9, $1f, $3b, $8c, $88, $95, $b9, $1c, $19, $9d
  .byte $30, $cc, $78, $2d, $a6, $28, $90, $b5, $ff

;level 6-4
E_CastleArea6:
  .byte $0f, $02, $09, $1f, $8b, $85, $2b, $8c, $e9, $1b, $25, $9d, $0f, $07, $09, $1d
  .byte $6d, $28, $99, $1b, $b5, $2c, $4b, $8c, $09, $9f, $fb, $15, $9d, $a8, $0f, $0c
  .byte $2b, $0c, $78, $2d, $90, $b5, $ff

;level 7-4
E_CastleArea7:
  .byte $05, $9d, $0d, $a8, $dd, $1d, $07, $ac, $54, $2c, $a2, $2c, $f4, $2c, $42, $ac
  .byte $26, $9d, $d4, $03, $24, $83, $64, $03, $2b, $82, $4b, $02, $7b, $02, $9b, $02
  .byte $5b, $82, $7b, $02, $0b, $82, $2b, $02, $c6, $1b, $28, $82, $48, $02, $a6, $1b
  .byte $7b, $95, $85, $0c, $9d, $9b, $0f, $0e, $78, $2d, $7a, $1d, $90, $b5, $ff

;level 8-4
E_CastleArea8:
  .byte $19, $9b, $99, $1b, $2c, $8c, $59, $1b, $c5, $0f, $0e, $83, $e0, $0f, $06, $2e
  .byte $67, $e7, $0f, $08, $9b, $07, $0e, $83, $e0, $39, $0e, $87, $10, $bd, $28, $59
  .byte $9f, $0f, $0f, $34, $0f, $77, $10, $9e, $67, $f1, $0f, $12, $0e, $67, $e3, $78
  .byte $2d, $0f, $15, $3b, $29, $57, $82, $0f, $18, $55, $1d, $78, $2d, $90, $b5, $ff

;level 5-1
E_GroundArea12:
  .byte $1b, $82, $4b, $02, $7b, $02, $ab, $02, $0f, $03, $f9, $0e, $d0, $be, $8e, $c4
  .byte $86, $f8, $0e, $c0, $ba, $0f, $0d, $3a, $0e, $bb, $02, $30, $b7, $80, $bc, $c0
  .byte $bc, $0f, $12, $24, $0f, $54, $0f, $ce, $3c, $80, $d3, $0f, $cb, $8e, $f9, $0e
  .byte $ff

;level 5-3
E_GroundArea13:
  .byte $0a, $aa, $15, $8f, $44, $0f, $4e, $44, $80, $d8, $07, $57, $90, $0f, $06, $67
  .byte $24, $8b, $17, $b9, $24, $ab, $97, $16, $87, $2a, $28, $84, $0f, $57, $a9, $a5
  .byte $29, $f5, $29, $a7, $a4, $0a, $a4, $ff

;level 6-1
E_GroundArea14:
  .byte $07, $82, $67, $0e, $40, $bd, $e0, $38, $d0, $bc, $6e, $84, $a0, $9b, $05, $0f
  .byte $06, $bb, $05, $0f, $08, $0b, $0e, $4b, $0e, $0f, $0a, $05, $29, $85, $29, $0f
  .byte $0c, $dd, $28, $ff

;level 6-3
E_GroundArea15:
  .byte $0f, $02, $28, $10, $e6, $03, $d8, $90, $0f, $05, $85, $0f, $78, $83, $c8, $10
  .byte $18, $83, $58, $83, $f7, $90, $0f, $0c, $43, $0f, $73, $8f, $ff

;level 7-1
E_GroundArea16:
  .byte $a7, $83, $d7, $03, $0f, $03, $6b, $00, $0f, $06, $e3, $0f, $14, $8f, $3e, $44
  .byte $c3, $0b, $80, $87, $05, $ab, $05, $db, $83, $0f, $0b, $07, $05, $13, $0e, $2b
  .byte $02, $4b, $02, $0f, $10, $0b, $0e, $b0, $37, $90, $bc, $80, $bc, $ae, $44, $c0
  .byte $ff

;level 7-2
E_GroundArea17:
  .byte $0a, $aa, $d5, $8f, $03, $8f, $3e, $44, $c6, $d8, $83, $0f, $06, $a6, $11, $b9
  .byte $0e, $39, $9d, $79, $1b, $a6, $11, $e8, $03, $87, $83, $16, $90, $a6, $11, $b9
  .byte $1d, $05, $8f, $38, $29, $89, $29, $26, $8f, $46, $29, $ff

;level 7-3
E_GroundArea18:
  .byte $0f, $04, $a3, $10, $0f, $09, $e3, $29, $0f, $0d, $55, $24, $a9, $24, $0f, $11
  .byte $59, $1d, $a9, $1b, $23, $8f, $15, $9b, $ff

;level 8-1
E_GroundArea19:
  .byte $0f, $01, $db, $02, $30, $b7, $80, $3b, $1b, $8e, $4a, $0e, $eb, $03, $3b, $82
  .byte $5b, $02, $e5, $0f, $14, $8f, $44, $0f, $5b, $82, $0c, $85, $35, $8f, $06, $85
  .byte $e3, $05, $db, $83, $3e, $84, $e0, $ff

;level 8-2
E_GroundArea22:
  .byte $0f, $02, $0a, $29, $f7, $02, $80, $bc, $6b, $82, $7b, $02, $9b, $02, $ab, $02
  .byte $39, $8e, $0f, $07, $ce, $35, $ec, $f5, $0f, $fb, $85, $fb, $85, $3e, $c4, $e3
  .byte $a7, $02, $ff

;level 8-3
E_GroundArea23:
  .byte $09, $a9, $86, $11, $d5, $10, $a3, $8f, $d5, $29, $86, $91, $2b, $83, $58, $03
  .byte $5b, $85, $eb, $05, $3e, $bc, $e0, $0f, $09, $43, $0f, $74, $0f, $6b, $85, $db
  .byte $05, $c6, $a4, $19, $a4, $12, $8f
;another unused area
E_GroundArea24:
  .byte $ff

;cloud level used with level 5-1
E_GroundArea29:
  .byte $0a, $aa, $2e, $2b, $98, $2e, $36, $e7, $ff

;level 5-2
E_UndergroundArea4:
  .byte $0b, $83, $b7, $03, $d7, $03, $0f, $05, $67, $03, $7b, $02, $9b, $02, $80, $b9
  .byte $3b, $83, $4e, $b4, $80, $86, $2b, $c9, $2c, $16, $ac, $67, $b4, $de, $3b, $81
  .byte $ff

;underground bonus rooms used with worlds 5-8
E_UndergroundArea5:
  .byte $1e, $af, $ca, $1e, $2c, $85, $0f, $04, $1e, $2d, $a7, $1e, $2f, $ce, $1e, $35
  .byte $e5, $0f, $07, $1e, $2b, $87, $1e, $30, $c5, $ff

;level 6-2
E_WaterArea2:
  .byte $0f, $01, $2e, $3b, $a1, $5b, $07, $ab, $07, $69, $87, $ba, $07, $fb, $87, $65
  .byte $a7, $6a, $27, $a6, $a7, $ac, $27, $1b, $87, $88, $07, $2b, $83, $7b, $07, $a7
  .byte $90, $e5, $83, $14, $a7, $19, $27, $77, $07, $f8, $07, $47, $8f, $b9, $07, $ff

;water area used in level 8-4
E_WaterArea4:
  .byte $07, $9b, $0a, $07, $b9, $1b, $66, $9b, $78, $07, $ae, $67, $e5, $ff

;water area used in level 6-1
E_WaterArea5:
  .byte $97, $87, $cb, $00, $ee, $2b, $f8, $fe, $2d, $ad, $75, $87, $d3, $27, $d9, $27
  .byte $0f, $04, $56, $0f, $ff

;level 5-4
L_CastleArea5:
  .byte $9b, $07, $05, $32, $06, $33, $07, $33, $3e, $03, $4c, $50, $4e, $07, $57, $31
  .byte $6e, $03, $7c, $52, $9e, $07, $fe, $0a, $7e, $89, $9e, $0a, $ee, $09, $fe, $0b
  .byte $13, $8e, $1e, $09, $3e, $0a, $6e, $09, $87, $0e, $9e, $02, $c6, $07, $ca, $0e
  .byte $f7, $62, $07, $8e, $08, $61, $17, $62, $1e, $0a, $4e, $06, $5e, $0a, $7e, $06
  .byte $8e, $0a, $ae, $06, $be, $07, $f3, $0e, $1e, $86, $2e, $0a, $84, $37, $93, $36
  .byte $a2, $45, $1e, $89, $46, $0e, $6e, $0a, $a7, $31, $db, $60, $f7, $60, $1b, $e0
  .byte $37, $31, $7e, $09, $8e, $0b, $a3, $0e, $fe, $04, $17, $bb, $47, $0e, $77, $0e
  .byte $be, $02, $ce, $0a, $07, $8e, $17, $31, $63, $31, $a7, $34, $c7, $0e, $13, $b1
  .byte $4e, $09, $1e, $8a, $7e, $02, $97, $34, $b7, $0e, $ce, $0a, $de, $02, $d8, $61
  .byte $f7, $62, $fe, $03, $07, $b4, $17, $0e, $47, $62, $4e, $0a, $5e, $03, $51, $61
  .byte $67, $62, $77, $34, $b7, $62, $c1, $61, $da, $60, $e9, $61, $f8, $62, $fe, $0a
  .byte $0d, $c4, $01, $52, $11, $52, $21, $52, $31, $52, $41, $52, $51, $52, $61, $52
  .byte $cd, $43, $ce, $09, $de, $0b, $dd, $42, $fe, $02, $5d, $c7, $fd

;level 6-4
L_CastleArea6:
  .byte $5b, $09, $05, $32, $06, $33, $4e, $0a, $87, $31, $fe, $02, $88, $f2, $c7, $33
  .byte $0d, $02, $07, $0e, $17, $34, $6e, $0a, $8e, $02, $bf, $67, $ed, $4b, $b7, $b6
  .byte $c3, $35, $1e, $8a, $2e, $02, $33, $3f, $37, $3f, $88, $f2, $c7, $33, $ed, $4b
  .byte $0d, $06, $03, $33, $0f, $74, $47, $73, $67, $73, $7e, $09, $9e, $0a, $ed, $4b
  .byte $f7, $32, $07, $8e, $97, $0e, $ae, $00, $de, $02, $e3, $35, $e7, $35, $3e, $8a
  .byte $4e, $02, $53, $3e, $57, $3e, $07, $8e, $a7, $34, $bf, $63, $ed, $4b, $2e, $8a
  .byte $fe, $06, $2e, $88, $34, $33, $35, $33, $6e, $06, $8e, $0c, $be, $06, $fe, $0a
  .byte $01, $d2, $0d, $44, $11, $52, $21, $52, $31, $52, $41, $52, $42, $0b, $51, $52
  .byte $61, $52, $cd, $43, $ce, $09, $dd, $42, $de, $0b, $fe, $02, $5d, $c7, $fd

;level 7-4
L_CastleArea7:
  .byte $58, $07, $05, $35, $06, $3d, $07, $3d, $be, $06, $de, $0c, $f3, $3d, $03, $8e
  .byte $6e, $43, $ce, $0a, $e1, $67, $f1, $67, $01, $e7, $11, $67, $1e, $05, $28, $39
  .byte $6e, $40, $be, $01, $c7, $06, $db, $0e, $de, $00, $1f, $80, $6f, $00, $bf, $00
  .byte $0f, $80, $5f, $00, $7e, $05, $a8, $37, $fe, $02, $24, $8e, $34, $30, $3e, $0c
  .byte $4e, $43, $ae, $0a, $be, $0c, $ee, $0a, $fe, $0c, $2e, $8a, $3e, $0c, $7e, $02
  .byte $8e, $0e, $98, $36, $b9, $34, $08, $bf, $09, $3f, $0e, $82, $2e, $86, $4e, $0c
  .byte $9e, $09, $c1, $62, $c4, $0e, $ee, $0c, $0e, $86, $5e, $0c, $7e, $09, $a1, $62
  .byte $a4, $0e, $ce, $0c, $fe, $0a, $28, $b4, $a6, $31, $e8, $34, $8b, $b2, $9b, $0e
  .byte $fe, $07, $fe, $8a, $0d, $c4, $cd, $43, $ce, $09, $dd, $42, $de, $0b, $fe, $02
  .byte $5d, $c7, $fd

;level 8-4
L_CastleArea8:
  .byte $5b, $03, $05, $34, $06, $35, $07, $36, $6e, $0a, $ee, $02, $fe, $05, $0d, $01
  .byte $17, $0e, $97, $0e, $9e, $02, $c6, $05, $fa, $30, $fe, $0a, $4e, $82, $57, $0e
  .byte $58, $62, $68, $62, $79, $61, $8a, $60, $8e, $0a, $f5, $31, $f9, $7b, $39, $f3
  .byte $97, $33, $b5, $71, $39, $f3, $4d, $48, $9e, $02, $ae, $05, $cd, $4a, $ed, $4b
  .byte $0e, $81, $17, $06, $39, $73, $5c, $02, $85, $65, $95, $32, $a9, $7b, $cc, $03
  .byte $5e, $8f, $6d, $47, $fe, $02, $0d, $07, $39, $73, $4e, $0a, $ae, $02, $ec, $71
  .byte $07, $81, $17, $02, $39, $73, $e6, $05, $39, $fb, $4e, $0a, $c4, $31, $eb, $61
  .byte $fe, $02, $07, $b0, $1e, $0a, $4e, $06, $57, $0e, $be, $02, $c9, $61, $da, $60
  .byte $ed, $4b, $0e, $85, $0d, $0e, $fe, $0a, $78, $e4, $8e, $06, $b3, $06, $bf, $47
  .byte $ee, $0f, $6d, $c7, $0e, $82, $39, $73, $9a, $60, $a9, $61, $ae, $06, $de, $0a
  .byte $e7, $03, $eb, $79, $f7, $03, $fe, $06, $0d, $14, $fe, $0a, $5e, $82, $7f, $66
  .byte $9e, $0a, $f8, $64, $fe, $0b, $9e, $84, $be, $05, $be, $82, $da, $60, $e9, $61
  .byte $f8, $62, $fe, $0a, $0d, $c4, $11, $64, $51, $62, $cd, $43, $ce, $09, $dd, $42
  .byte $de, $0b, $fe, $02, $5d, $c7, $fd

;level 5-1
L_GroundArea12:
  .byte $52, $b1, $0f, $20, $6e, $75, $cc, $73, $a3, $b3, $bf, $74, $0c, $84, $83, $3f
  .byte $9f, $74, $ef, $71, $ec, $01, $2f, $f1, $2c, $01, $6f, $71, $6c, $01, $a8, $91
  .byte $aa, $10, $77, $fb, $56, $f4, $39, $f1, $bf, $37, $33, $e7, $43, $04, $47, $03
  .byte $6c, $05, $c3, $67, $d3, $67, $e3, $67, $ed, $4c, $fc, $07, $73, $e7, $83, $67
  .byte $93, $67, $a3, $67, $bc, $08, $43, $e7, $53, $67, $dc, $02, $59, $91, $c3, $33
  .byte $d9, $71, $df, $72, $2d, $cd, $5b, $71, $9b, $71, $3b, $f1, $a7, $c2, $db, $71
  .byte $0d, $10, $9b, $71, $0a, $b0, $1c, $04, $67, $63, $76, $64, $85, $65, $94, $66
  .byte $a3, $67, $b3, $67, $cc, $09, $73, $a3, $87, $22, $b3, $09, $d6, $83, $e3, $03
  .byte $fe, $3f, $0d, $15, $de, $31, $ec, $01, $03, $f7, $9d, $41, $df, $26, $0d, $18
  .byte $39, $71, $7f, $37, $f2, $68, $01, $e9, $11, $39, $68, $7a, $de, $3f, $6d, $c5
  .byte $fd

;level 5-3
L_GroundArea13:
  .byte $50, $11, $0f, $26, $df, $32, $fe, $10, $0d, $01, $98, $74, $c8, $13, $52, $e1
  .byte $63, $31, $61, $79, $c6, $61, $06, $e1, $8b, $71, $ab, $71, $e4, $19, $eb, $19
  .byte $60, $86, $c8, $13, $cd, $4b, $39, $f3, $98, $13, $17, $f5, $7c, $15, $7f, $13
  .byte $cf, $15, $d4, $40, $0b, $9a, $23, $16, $32, $44, $a3, $95, $b2, $43, $0d, $0a
  .byte $27, $14, $3d, $4a, $a4, $40, $bc, $16, $bf, $13, $c4, $40, $04, $c0, $1f, $16
  .byte $24, $40, $43, $31, $ce, $11, $dd, $41, $0e, $d2, $3f, $20, $3d, $c7, $fd
 
;level 6-1
L_GroundArea14:
  .byte $52, $a1, $0f, $20, $6e, $40, $d6, $61, $e7, $07, $f7, $21, $16, $e1, $34, $63
  .byte $47, $21, $54, $04, $67, $0a, $74, $63, $dc, $01, $06, $e1, $17, $26, $86, $61
  .byte $66, $c2, $58, $c1, $f7, $03, $04, $f6, $8a, $10, $9c, $04, $e8, $62, $f9, $61
  .byte $0a, $e0, $53, $31, $5f, $73, $7b, $71, $77, $25, $fc, $e2, $17, $aa, $23, $00
  .byte $3c, $67, $b3, $01, $cc, $63, $db, $71, $df, $73, $fc, $00, $4f, $b7, $ca, $7a
  .byte $c5, $31, $ec, $54, $3c, $dc, $5d, $4c, $0f, $b3, $47, $63, $6b, $f1, $8c, $0a
  .byte $39, $f1, $ec, $03, $f0, $33, $0f, $e2, $29, $73, $49, $61, $58, $62, $67, $73
  .byte $85, $65, $94, $66, $a3, $77, $ad, $4d, $4d, $c1, $6f, $26, $5d, $c7, $fd

;level 6-3
L_GroundArea15:
  .byte $50, $11, $0f, $26, $af, $32, $d8, $62, $de, $10, $08, $e4, $5a, $62, $6c, $4c
  .byte $86, $43, $ad, $48, $3a, $e2, $53, $42, $88, $64, $9c, $36, $08, $e4, $4a, $62
  .byte $5c, $4d, $3a, $e2, $9c, $32, $fc, $41, $3c, $b1, $83, $00, $ac, $42, $2a, $e2
  .byte $3c, $46, $aa, $62, $bc, $4e, $c6, $43, $46, $c3, $aa, $62, $bd, $48, $0b, $96
  .byte $47, $07, $c7, $12, $3c, $c2, $9c, $41, $cd, $48, $dc, $32, $4c, $c2, $bc, $32
  .byte $1c, $b1, $5a, $62, $6c, $44, $76, $43, $ba, $62, $dc, $32, $5d, $ca, $73, $12
  .byte $e3, $12, $8e, $91, $9d, $41, $be, $42, $ef, $20, $cd, $c7, $fd

;level 7-1
L_GroundArea16:
  .byte $52, $b1, $0f, $20, $6e, $76, $03, $b1, $09, $71, $0f, $71, $6f, $33, $a7, $63
  .byte $b7, $34, $bc, $0e, $4d, $cc, $03, $a6, $08, $72, $3f, $72, $6d, $4c, $73, $07
  .byte $77, $73, $83, $27, $ac, $00, $bf, $73, $3c, $80, $9a, $30, $ac, $5b, $c6, $3c
  .byte $6a, $b0, $75, $10, $96, $74, $b6, $0a, $da, $30, $e3, $28, $ec, $5b, $ed, $48
  .byte $aa, $b0, $33, $b4, $51, $79, $ad, $4a, $dd, $4d, $e3, $2c, $0c, $fa, $73, $07
  .byte $b3, $04, $cb, $71, $ec, $07, $0d, $0a, $39, $71, $df, $33, $ca, $b0, $d6, $10
  .byte $d7, $30, $dc, $0c, $03, $b1, $ad, $41, $ef, $26, $ed, $c7, $39, $f1, $0d, $10
  .byte $7d, $4c, $0d, $13, $a8, $11, $aa, $10, $1c, $83, $d7, $7b, $f3, $67, $5d, $cd
  .byte $6d, $47, $fd

;level 7-2
L_GroundArea17:
  .byte $56, $11, $0f, $26, $df, $32, $fe, $11, $0d, $01, $0c, $5f, $03, $80, $0c, $52
  .byte $29, $15, $7c, $5b, $23, $b2, $29, $1f, $31, $79, $1c, $de, $48, $3b, $ed, $4b
  .byte $39, $f1, $cf, $b3, $fe, $10, $37, $8e, $77, $0e, $9e, $11, $a8, $34, $a9, $34
  .byte $aa, $34, $f8, $62, $fe, $10, $37, $b6, $de, $11, $e7, $63, $f8, $62, $09, $e1
  .byte $0e, $10, $47, $36, $b7, $0e, $be, $91, $ca, $32, $ee, $10, $1d, $ca, $7e, $11
  .byte $83, $77, $9e, $10, $1e, $91, $2d, $41, $4f, $26, $4d, $c7, $fd

;level 7-3
L_GroundArea18:
  .byte $57, $11, $0f, $26, $fe, $10, $4b, $92, $59, $0f, $ad, $4c, $d3, $93, $0b, $94
  .byte $29, $0f, $7b, $93, $99, $0f, $0d, $06, $27, $12, $35, $0f, $23, $b1, $57, $75
  .byte $a3, $31, $ab, $71, $f7, $75, $23, $b1, $87, $13, $95, $0f, $0d, $0a, $23, $35
  .byte $38, $13, $55, $00, $9b, $16, $0b, $96, $c7, $75, $3b, $92, $49, $0f, $ad, $4c
  .byte $29, $92, $52, $40, $6c, $15, $6f, $11, $72, $40, $bf, $15, $03, $93, $0a, $13
  .byte $12, $41, $8b, $12, $99, $0f, $0d, $10, $47, $16, $46, $45, $b3, $32, $13, $b1
  .byte $57, $0e, $a7, $0e, $d3, $31, $53, $b1, $a6, $31, $03, $b2, $13, $0e, $8d, $4d
  .byte $ae, $11, $bd, $41, $ee, $52, $0f, $a0, $dd, $47, $fd

;level 8-1
L_GroundArea19:
  .byte $52, $a1, $0f, $20, $6e, $65, $57, $f3, $60, $21, $6f, $62, $ac, $75, $07, $80
  .byte $1c, $76, $87, $01, $9c, $70, $b0, $33, $cf, $66, $57, $e3, $6c, $04, $cd, $4c
  .byte $9a, $b0, $ac, $0c, $83, $b1, $8f, $74, $bd, $4d, $f8, $11, $fa, $10, $83, $87
  .byte $93, $22, $9f, $74, $59, $f1, $89, $61, $a9, $61, $bc, $0c, $67, $a0, $eb, $71
  .byte $77, $87, $7a, $10, $86, $51, $95, $52, $a4, $53, $b6, $04, $b3, $24, $26, $85
  .byte $4a, $10, $53, $23, $5c, $00, $6f, $73, $93, $08, $07, $fb, $2c, $04, $33, $30
  .byte $74, $76, $eb, $71, $57, $8b, $6c, $02, $96, $74, $e3, $30, $0c, $86, $7d, $41
  .byte $bf, $26, $bd, $c7, $fd

;level 8-2
L_GroundArea22:
  .byte $50, $61, $0f, $26, $bb, $f1, $dc, $06, $23, $87, $b5, $71, $b7, $31, $d7, $28
  .byte $06, $c5, $67, $08, $0d, $05, $39, $71, $7c, $00, $9e, $62, $b6, $0b, $e6, $08
  .byte $4e, $e0, $5d, $4c, $59, $0f, $6c, $02, $93, $67, $ac, $56, $ad, $4c, $1f, $b1
  .byte $3c, $01, $98, $0a, $9e, $20, $a8, $21, $f3, $09, $0e, $a1, $27, $20, $3e, $62
  .byte $56, $08, $7d, $4d, $c6, $08, $3e, $e0, $9e, $62, $b6, $08, $1e, $e0, $4c, $00
  .byte $6c, $00, $a7, $7b, $de, $2f, $6d, $c7, $fe, $10, $0b, $93, $5b, $15, $b7, $12
  .byte $03, $91, $ab, $1f, $bd, $41, $ef, $26, $ad, $c7, $fd

;level 8-3
L_GroundArea23:
  .byte $50, $50, $0f, $26, $0b, $1f, $57, $92, $8b, $12, $d2, $14, $4b, $92, $59, $0f
  .byte $0b, $95, $bb, $1f, $be, $52, $58, $e2, $9e, $50, $97, $08, $bb, $1f, $ae, $d2
  .byte $b6, $08, $bb, $1f, $dd, $4a, $f6, $07, $26, $89, $8e, $50, $98, $62, $eb, $11
  .byte $07, $f3, $0b, $1d, $2e, $52, $47, $0a, $ce, $50, $eb, $1f, $ee, $52, $5e, $d0
  .byte $d9, $0f, $ab, $9f, $be, $52, $8e, $d0, $ab, $1d, $ae, $52, $36, $8b, $56, $08
  .byte $5e, $50, $dc, $15, $df, $12, $2f, $95, $c3, $31, $5b, $9f, $6d, $41, $8e, $52
  .byte $af, $20, $ad, $c7
;another unused area
L_GroundArea24:
  .byte $fd

;cloud level used with level 5-1
L_GroundArea29:
  .byte $00, $c1, $4c, $00, $f3, $4f, $fa, $c6, $68, $a0, $69, $20, $6a, $20, $7a, $47
  .byte $f8, $20, $f9, $20, $fa, $20, $0a, $cf, $b4, $49, $55, $a0, $56, $20, $73, $47
  .byte $f5, $20, $f6, $20, $22, $a1, $41, $48, $52, $20, $72, $20, $92, $20, $b2, $20
  .byte $fe, $00, $9b, $c2, $ad, $c7, $fd

;level 5-2
L_UndergroundArea4:
  .byte $48, $0f, $1e, $01, $27, $06, $5e, $02, $8f, $63, $8c, $01, $ef, $67, $1c, $81
  .byte $2e, $09, $3c, $63, $73, $01, $8c, $60, $fe, $02, $1e, $8e, $3e, $02, $44, $07
  .byte $45, $52, $4e, $0e, $8e, $02, $99, $71, $b5, $24, $b6, $24, $b7, $24, $fe, $02
  .byte $07, $87, $17, $22, $37, $52, $37, $0b, $47, $52, $4e, $0a, $57, $52, $5e, $02
  .byte $67, $52, $77, $52, $7e, $0a, $87, $52, $8e, $02, $96, $46, $97, $52, $a7, $52
  .byte $b7, $52, $c7, $52, $d7, $52, $e7, $52, $f7, $52, $fe, $04, $07, $a3, $47, $08
  .byte $57, $26, $c7, $0a, $e9, $71, $17, $a7, $97, $08, $9e, $01, $a0, $24, $c6, $74
  .byte $f0, $0c, $fe, $04, $0c, $80, $6f, $32, $98, $62, $a8, $62, $bc, $00, $c7, $73
  .byte $e7, $73, $fe, $02, $7f, $e7, $8e, $01, $9e, $00, $de, $02, $f7, $0b, $fe, $0e
  .byte $4e, $82, $54, $52, $64, $51, $6e, $00, $74, $09, $9f, $00, $df, $00, $2f, $80
  .byte $4e, $02, $59, $47, $ce, $0a, $07, $f5, $68, $54, $7f, $64, $88, $54, $a8, $54
  .byte $ae, $01, $b8, $52, $bf, $47, $c8, $52, $d8, $52, $e8, $52, $ee, $0f, $4d, $c7
  .byte $0d, $0d, $0e, $02, $68, $7a, $be, $01, $ee, $0f, $6d, $c5, $fd

;underground bonus rooms used with worlds 5-8
L_UndergroundArea5:
  .byte $08, $0f, $0e, $01, $2e, $05, $38, $2c, $3a, $4f, $08, $ac, $c7, $0b, $ce, $01
  .byte $df, $4a, $6d, $c7, $0e, $81, $00, $5a, $2e, $02, $b8, $4f, $cf, $65, $0f, $e5
  .byte $4f, $65, $8f, $65, $df, $4a, $6d, $c7, $0e, $81, $00, $5a, $30, $07, $34, $52
  .byte $3e, $02, $42, $47, $44, $47, $46, $27, $c0, $0b, $c4, $52, $df, $4a, $6d, $c7
  .byte $fd

;level 6-2
L_WaterArea2:
  .byte $41, $01, $27, $d3, $79, $51, $c4, $56, $00, $e2, $03, $53, $0c, $0f, $12, $3b
  .byte $1a, $42, $43, $54, $6d, $49, $83, $53, $99, $53, $c3, $54, $da, $52, $0c, $84
  .byte $09, $53, $53, $64, $63, $31, $67, $34, $86, $41, $8c, $01, $a3, $30, $b3, $64
  .byte $cc, $03, $d9, $42, $5c, $84, $a0, $62, $a8, $62, $b0, $62, $b8, $62, $c0, $62
  .byte $c8, $62, $d0, $62, $d8, $62, $e0, $62, $e8, $62, $16, $c2, $58, $52, $8c, $04
  .byte $a7, $55, $d0, $63, $d7, $65, $e2, $61, $e7, $65, $f2, $61, $f7, $65, $13, $b8
  .byte $17, $38, $8c, $03, $1d, $c9, $50, $62, $5c, $0b, $62, $3e, $63, $52, $8a, $52
  .byte $93, $54, $aa, $42, $d3, $51, $ea, $41, $03, $d3, $1c, $04, $1a, $52, $33, $55
  .byte $73, $44, $77, $44, $16, $d2, $19, $31, $1a, $32, $5c, $0f, $9a, $47, $95, $64
  .byte $a5, $64, $b5, $64, $c5, $64, $d5, $64, $e5, $64, $f5, $64, $05, $e4, $40, $61
  .byte $42, $35, $56, $34, $5c, $09, $a2, $61, $a6, $61, $b3, $34, $b7, $34, $fc, $08
  .byte $0c, $87, $28, $54, $59, $53, $9a, $30, $a9, $61, $b8, $62, $be, $0b, $d4, $60
  .byte $d5, $0d, $de, $0f, $0d, $ca, $7d, $47, $fd

;water area used in level 8-4
L_WaterArea4:
  .byte $07, $0f, $0e, $02, $39, $73, $05, $8e, $2e, $0b, $b7, $0e, $64, $8e, $6e, $02
  .byte $ce, $06, $de, $0f, $e6, $0d, $7d, $c7, $fd

;water area used in level 6-1
L_WaterArea5:
  .byte $01, $01, $77, $39, $a3, $43, $00, $bf, $29, $51, $39, $48, $61, $55, $d6, $54
  .byte $d2, $44, $0c, $82, $2e, $02, $31, $66, $44, $47, $47, $32, $4a, $47, $97, $32
  .byte $c1, $66, $ce, $01, $dc, $02, $fe, $0e, $0c, $8f, $08, $4f, $fe, $01, $27, $d3
  .byte $5c, $02, $9a, $60, $a9, $61, $b8, $62, $c7, $63, $ce, $0f, $d5, $0d, $7d, $c7
  .byte $fd

;unused bytes
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
.endif
