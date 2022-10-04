;SMB2J DISASSEMBLY (SM2DATA4 portion)

;-------------------------------------------------------------------------------------
;DEFINES

AreaData              = $e7
AreaDataLow           = $e7
AreaDataHigh          = $e8
EnemyData             = $e9
EnemyDataLow          = $e9
EnemyDataHigh         = $ea

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

TimerControl          = $0747
EnemyFrameTimer       = $078a

Sprite_Y_Position     = $0200
Sprite_Tilenumber     = $0201
Sprite_Attributes     = $0202
Sprite_X_Position     = $0203

Alt_SprDataOffset     = $06ec

NoiseSoundQueue       = $fd

TerrainControl        = $0727
AreaStyle             = $0733
ForegroundScenery     = $0741
BackgroundScenery     = $0742
CloudTypeOverride     = $0743
BackgroundColorCtrl   = $0744
AreaType              = $074e
AreaAddrsLOffset      = $074f
AreaPointer           = $0750

PlayerEntranceCtrl    = $0710
GameTimerSetting      = $0715
AltEntranceControl    = $0752
EntrancePage          = $0751

WorldNumber           = $075f
AreaNumber            = $0760 ;internal number used to find areas

; imports from other files
.import HalfwayPageNybbles
.import GetPipeHeight
.import FindEmptyEnemySlot
.import SetupPiranhaPlant
.import VerticalPipeData
.import RenderUnderPart
.import MetatileBuffer
.import GetAreaType
.ifdef ANN
.import E_HArea10
.import E_HArea11
.import L_HArea10
.import L_HArea11
.else
.import E_GroundArea21
.import E_GroundArea28
.import L_GroundArea10
.import L_GroundArea28
.endif

; exports to other files
.export UpsideDownPipe_High
.export UpsideDownPipe_Low
.ifndef ANN
.export WindOn
.export WindOff
.export SimulateWind
.export BlowPlayerAround
.endif
.export MoveUpsideDownPiranhaP
.export ChangeHalfwayPages

;-------------------------------------------------------------------------------------------------

FindAreaPointer:
      ldy WorldNumber        ;load offset from world variable
      lda WorldAddrOffsets,y
      clc                    ;add area number used to find data
      adc AreaNumber
      tay
      lda AreaAddrOffsets,y  ;from there we have our area pointer
      rts

GetAreaDataAddrs:
            lda AreaPointer          ;use 2 MSB for Y
            jsr GetAreaType
            tay
            lda AreaPointer          ;mask out all but 5 LSB
            and #%00011111
            sta AreaAddrsLOffset     ;save as low offset
            lda EnemyAddrHOffsets,y  ;load base value with 2 altered MSB,
            clc                      ;then add base value to 5 LSB, result
            adc AreaAddrsLOffset     ;becomes offset for level data
            asl
            tay
            lda EnemyDataAddrs+1,y   ;use offset to load pointer
            sta EnemyDataHigh
            lda EnemyDataAddrs,y
            sta EnemyDataLow
            ldy AreaType             ;use area type as offset
            lda AreaDataHOffsets,y   ;do the same thing but with different base value
            clc
            adc AreaAddrsLOffset
            asl
            tay
            lda AreaDataAddrs+1,y    ;use this offset to load another pointer
            sta AreaDataHigh
            lda AreaDataAddrs,y
            sta AreaDataLow
            ldy #$00                 ;load first byte of header
            lda (AreaData),y     
            pha                      ;save it to the stack for now
            and #%00000111           ;save 3 LSB for foreground scenery or bg color control
            cmp #$04
            bcc StoreFore
            sta BackgroundColorCtrl  ;if 4 or greater, save value here as bg color control
            lda #$00
StoreFore:  sta ForegroundScenery    ;if less, save value here as foreground scenery
            pla                      ;pull byte from stack and push it back
            pha
            and #%00111000           ;save player entrance control bits
            lsr                      ;shift bits over to LSBs
            lsr
            lsr
            sta PlayerEntranceCtrl   ;save value here as player entrance control
            pla                      ;pull byte again but do not push it back
            and #%11000000           ;save 2 MSB for game timer setting
            clc
            rol                      ;rotate bits over to LSBs
            rol
            rol
            sta GameTimerSetting     ;save value here as game timer setting
            iny
            lda (AreaData),y         ;load second byte of header
            pha                      ;save to stack
            and #%00001111           ;mask out all but lower nybble
            sta TerrainControl
            pla                      ;pull and push byte to copy it to A
            pha
            and #%00110000           ;save 2 MSB for background scenery type
            lsr
            lsr                      ;shift bits to LSBs
            lsr
            lsr
            sta BackgroundScenery    ;save as background scenery
            pla           
            and #%11000000
            clc
            rol                      ;rotate bits over to LSBs
            rol
            rol
            cmp #%00000011           ;if set to 3, store here
            bne StoreStyle           ;and nullify other value
            sta CloudTypeOverride    ;otherwise store value in other place
            lda #$00
StoreStyle: sta AreaStyle
            lda AreaDataLow          ;increment area data address by 2 bytes
            clc
            adc #$02
            sta AreaDataLow
            lda AreaDataHigh
            adc #$00
            sta AreaDataHigh
            rts

WorldAddrOffsets:
     .byte WorldAAreas-AreaAddrOffsets, WorldBAreas-AreaAddrOffsets
     .byte WorldCAreas-AreaAddrOffsets, WorldDAreas-AreaAddrOffsets
     .byte 0,0,0,0
.ifndef ANN
     .byte 0
.endif

AreaAddrOffsets:
WorldAAreas: .byte $20, $2c, $40, $21, $60
WorldBAreas: .byte $22, $2c, $00, $23, $61
WorldCAreas: .byte $24, $25, $26, $62
WorldDAreas: .byte $27, $28, $29, $63

EnemyAddrHOffsets:
     .byte $14, $04, $12, $00

EnemyDataAddrs:
.ifdef ANN
     .addr E_HArea00,E_HArea01,E_HArea02,E_HArea03,E_HArea04,E_HArea05,E_HArea06,E_HArea07
     .addr E_HArea08,E_HArea09,E_HArea0A,E_HArea0B,E_HArea0C,E_HArea0D,E_HArea0E,E_HArea0F
     .addr E_HArea10,E_HArea11,E_HArea12,E_HArea13,E_HArea14
.else
     .word E_CastleArea11, E_CastleArea12, E_CastleArea13, E_CastleArea14, E_GroundArea30, E_GroundArea31
     .word E_GroundArea32, E_GroundArea33, E_GroundArea34, E_GroundArea35, E_GroundArea36, E_GroundArea37
     .word E_GroundArea38, E_GroundArea39, E_GroundArea40, E_GroundArea41, E_GroundArea21, E_GroundArea28
     .word E_UndergroundArea6, E_UndergroundArea7, E_WaterArea9
.endif

AreaDataHOffsets:
     .byte $14, $04, $12, $00

AreaDataAddrs:
.ifdef ANN
     .addr L_HArea00,L_HArea01,L_HArea02,L_HArea03,L_HArea04,L_HArea05,L_HArea06,L_HArea07
     .addr L_HArea08,L_HArea09,L_HArea0A,L_HArea0B,L_HArea0C,L_HArea0D,L_HArea0E,L_HArea0F
     .addr L_HArea10,L_HArea11,L_HArea12,L_HArea13,L_HArea14
.else
     .word L_CastleArea11, L_CastleArea12, L_CastleArea13, L_CastleArea14, L_GroundArea30, L_GroundArea31
     .word L_GroundArea32, L_GroundArea33, L_GroundArea34, L_GroundArea35, L_GroundArea36, L_GroundArea37
     .word L_GroundArea38, L_GroundArea39, L_GroundArea40, L_GroundArea41, L_GroundArea10, L_GroundArea28
     .word L_UndergroundArea6, L_UndergroundArea7, L_WaterArea9
.endif

AtoDHalfwayPages:
     .byte $76, $50
.ifdef ANN
     .byte $D5, $70
.else
     .byte $65, $50
.endif
     .byte $75, $b0
     .byte $00, $00

ChangeHalfwayPages:
        ldy #$07
CHalfL: lda AtoDHalfwayPages,y     ;load new halfway nybbles over the old ones
        sta HalfwayPageNybbles,y
        dey
        bpl CHalfL
        rts

; unused space
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.ifdef ANN
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
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
       sta PiranhaPlantDownYPos,x   ;set as "down" position, which in this case is up
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

       rts                       ;unused, nothing jumps here

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

.ifndef ANN
BlowPlayerAround:
        lda WindFlag            ;if wind is turned off, just exit
        beq ExBlow
        lda AreaType            ;don't blow the player around unless
        cmp #$01                ;the area is ground type
        bne ExBlow
        ldy #$01
        lda FrameCounter
        asl
        bcs BThr
        ldy #$03
BThr:   sty $00
        lda FrameCounter
        and $00
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
          ldx #$00
          ldy Alt_SprDataOffset-1  ;use first sprite data offset for first six leaves
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
          inx
          cpx #$06                 ;if still on first six leaves, continue
          bne DLLoop               ;using the first sprite data offset
          ldy Alt_SprDataOffset    ;otherwise use the second one instead
DLLoop:   cpx #$0c                 ;continue until done putting twelve leaves on the screen
          bne DrawLeaf
ExSimW:   rts

LeavesPosAdder:
   .byte $57, $57, $56, $56, $58, $58, $56, $56, $57, $58, $57, $58
   .byte $59, $59, $58, $58, $5a, $5a, $58, $58, $59, $5a, $59, $5a

ModifyLeavesPos:
         ldx #$0b
MLPLoop: lda LeavesXPos,x     ;add each adder to each X position twice
         clc                  ;and to each Y position once
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
     lda #$01       ;branch to turn the wind on
     bne WOn
WindOff:
     lda #$00       ;turn the wind off
WOn: sta WindFlag
     rts

;some unused bytes
   .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
.endif

.ifdef ANN
AnnSomething:
  ldy WorldNumber
  cpy #$01
  beq @Shift
  cpy #$02
  bne @Done
@Shift:
  lda #$E0
@Done:
  rts

AnnSomethingAgain:
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
.endif



.ifdef ANN
E_HArea00:
.byte $2A, $9E, $6B, $0C, $8D, $1C, $EA, $1F, $1B, $8C, $E6, $1C, $8C, $9C, $BB, $0C
.byte $F3, $83, $9B, $8C, $DB, $0C, $1B, $8C, $6B, $0C, $BB, $0C, $0F, $09, $40, $15
.byte $78, $AD, $90, $B5, $FF

E_HArea01:
.byte $0F, $02, $38, $1D, $D9, $1B, $6E, $E1, $21, $3A, $A8, $18, $9D, $0F, $07, $18
.byte $1D, $0F, $09, $18, $1D, $0F, $0B, $18, $1D, $7B, $15, $8E, $21, $2E, $B9, $9D
.byte $0F, $0E, $78, $2D, $90, $B5, $FF

E_HArea02:
.byte $05, $9D, $0D, $A8, $DD, $1D, $07, $AC, $54, $2C, $A2, $2C, $F4, $2C, $42, $AC
.byte $26, $9D, $D4, $03, $24, $83, $64, $03, $2B, $82, $4B, $02, $7B, $02, $9B, $02
.byte $5B, $82, $7B, $02, $0B, $82, $2B, $02, $C6, $1B, $28, $82, $48, $02, $A6, $1B
.byte $7B, $95, $85, $0C, $9D, $9B, $0F, $0E, $78, $2D, $7A, $1D, $90, $B5, $FF

E_HArea03:
.byte $19, $9F, $99, $1B, $2C, $8C, $59, $1B, $C5, $0F, $0F, $04, $09, $29, $BD, $1D
.byte $0F, $06, $6E, $2A, $61, $0F, $09, $48, $2D, $46, $87, $79, $07, $8E, $63, $60
.byte $A5, $07, $B8, $85, $57, $A5, $8C, $8C, $76, $9D, $78, $2D, $90, $B5, $FF

E_HArea04:
.byte $07, $83, $37, $03, $6B, $0E, $E0, $3D, $20, $BE, $6E, $2B, $00, $A7, $85, $D3
.byte $05, $E7, $83, $24, $83, $27, $03, $49, $00, $59, $00, $10, $BB, $B0, $3B, $6E
.byte $C1, $00, $17, $85, $53, $05, $36, $8E, $76, $0E, $B6, $0E, $E7, $83, $63, $83
.byte $68, $03, $29, $83, $57, $03, $85, $03, $B5, $29, $FF

E_Area1E:
E_HArea05:
.byte $56, $87, $44, $87, $0F, $04, $66, $87, $0F, $06, $86, $10, $0F, $08, $55, $0F
.byte $E5, $8F, $FF

E_HArea06:
.byte $1B, $82, $4B, $02, $7B, $02, $AB, $02, $0F, $03, $F9, $0E, $D0, $BE, $8E, $C1
.byte $24, $F8, $0E, $C0, $BA, $0F, $0D, $3A, $0E, $BB, $02, $30, $B7, $80, $BC, $C0
.byte $BC, $0F, $12, $24, $0F, $54, $0F, $CE, $2B, $20, $D3, $0F, $CB, $8E, $F9, $0E
.byte $FF

E_HArea07:
.byte $80, $BE, $83, $03, $92, $10, $4B, $80, $B0, $3C, $07, $80, $B7, $24, $0C, $A4
.byte $96, $A9, $1B, $83, $7B, $24, $B7, $24, $97, $83, $E2, $0F, $A9, $A9, $38, $A9
.byte $0F, $0B, $74, $0F, $FF

E_HArea08:
.byte $3A, $8E, $5B, $0E, $C3, $8E, $CA, $8E, $0B, $8E, $4A, $0E, $DE, $C1, $44, $0F
.byte $08, $49, $0E, $EB, $0E, $8A, $90, $AB, $85, $0F, $0C, $03, $0F, $2E, $2B, $40
.byte $67, $86, $FF

E_HArea09:
.byte $15, $8F, $54, $07, $AA, $83, $F8, $07, $0F, $04, $14, $07, $96, $10, $0F, $07
.byte $95, $0F, $9D, $A8, $0B, $97, $09, $A9, $55, $24, $A9, $24, $BB, $17, $FF

E_HArea0A:
.byte $0F, $04, $A3, $10, $0F, $09, $E3, $29, $0F, $0D, $55, $24, $A9, $24, $0F, $11
.byte $59, $1D, $A9, $1B, $23, $8F, $15, $9B, $FF

E_HArea0B:
.byte $DB, $82, $30, $B7, $80, $3B, $1B, $8E, $4A, $0E, $EB, $03, $3B, $82, $5B, $02
.byte $E5, $0F, $14, $8F, $44, $0F, $5E, $41, $60, $5B, $82, $0C, $85, $35, $8F, $06
.byte $85, $E3, $05, $2E, $AB, $60, $DB, $03, $FF

E_HArea0C:
.byte $DB, $82, $F3, $03, $10, $B7, $80, $37, $1A, $8E, $4B, $0E, $7A, $0E, $AB, $0E
.byte $0F, $05, $F9, $0E, $D0, $BE, $2E, $C1, $62, $D4, $8F, $64, $8F, $7E, $2B, $60
.byte $FF

E_HArea0D:
.byte $0F, $03, $AB, $05, $1B, $85, $A3, $85, $D7, $05, $0F, $08, $33, $03, $0B, $85
.byte $FB, $85, $8B, $85, $3A, $8E, $FF

E_HArea0E:
.byte $0F, $02, $09, $05, $3E, $41, $64, $2B, $8E, $58, $0E, $CA, $07, $34, $87, $FF

E_HArea0F:
.byte $0A, $AA, $1E, $20, $03, $1E, $22, $30, $2E, $24, $48, $2E, $28, $67, $FF

E_HArea12:
.byte $BB, $A9, $1B, $A9, $69, $29, $B8, $29, $59, $A9, $8D, $A8, $0F, $07, $15, $29
.byte $55, $AC, $6B, $85, $0E, $AD, $01, $67, $34, $FF

E_HArea13:
.byte $1E, $A0, $09, $1E, $27, $61, $0F, $03, $1E, $28, $68, $1E, $22, $27, $0F, $05
.byte $1E, $24, $48, $1E, $63, $68, $FF

E_HArea14:
.byte $EE, $AD, $21, $26, $87, $F3, $0E, $66, $87, $CB, $00, $65, $87, $0F, $06, $06
.byte $0E, $97, $07, $CB, $00, $75, $87, $D3, $27, $D9, $27, $0F, $09, $77, $1F, $46
.byte $87, $B1, $0F, $FF




L_HArea00:
.byte $9B, $87, $05, $32, $06, $33, $07, $34, $EE, $0A, $0E, $86, $28, $0B, $3E, $0A
.byte $6E, $02, $8B, $0B, $97, $00, $9E, $0A, $CE, $06, $E8, $0B, $FE, $0A, $2E, $86
.byte $6E, $0A, $8E, $08, $E4, $0B, $1E, $82, $8A, $0B, $8E, $0A, $FE, $02, $1A, $E0
.byte $29, $61, $2E, $06, $3E, $09, $56, $60, $65, $61, $6E, $0C, $83, $60, $7E, $8A
.byte $BB, $61, $F9, $63, $27, $E5, $88, $64, $EB, $61, $FE, $05, $68, $90, $0A, $90
.byte $FE, $02, $3A, $90, $3E, $0A, $AE, $02, $DA, $60, $E9, $61, $F8, $62, $FE, $0A
.byte $0D, $C4, $A1, $62, $B1, $62, $CD, $43, $CE, $09, $DE, $0B, $DD, $42, $FE, $02
.byte $5D, $C7, $FD

L_HArea01:
.byte $9B, $07, $05, $32, $06, $33, $07, $33, $3E, $0A, $41, $3B, $42, $3B, $58, $64
.byte $7A, $62, $C8, $31, $18, $E4, $39, $73, $5E, $09, $66, $3C, $0E, $82, $28, $05
.byte $36, $0B, $3E, $0A, $AE, $02, $D7, $0B, $FE, $0C, $FE, $8A, $11, $E5, $21, $65
.byte $31, $65, $4E, $0C, $FE, $02, $16, $8B, $2E, $0E, $FE, $02, $18, $FA, $3E, $0E
.byte $FE, $02, $16, $8B, $2E, $0E, $FE, $02, $18, $FA, $3E, $0E, $FE, $02, $16, $8B
.byte $2E, $0E, $FE, $02, $18, $FA, $3E, $0E, $FE, $02, $16, $8B, $2E, $0E, $FE, $02
.byte $18, $FA, $5E, $0A, $6E, $02, $7E, $0A, $B7, $0B, $EE, $07, $FE, $8A, $0D, $C4
.byte $CD, $43, $CE, $09, $DD, $42, $DE, $0B, $FE, $02, $5D, $C7, $FD

L_HArea02:
.byte $58, $07, $05, $35, $06, $3D, $07, $3D, $BE, $06, $DE, $0C, $F3, $3D, $03, $8B
.byte $6E, $43, $CE, $0A, $E1, $67, $F1, $67, $01, $E7, $11, $67, $1E, $05, $28, $39
.byte $6E, $40, $BE, $01, $C7, $04, $DB, $0B, $DE, $00, $1F, $80, $6F, $00, $BF, $00
.byte $0F, $80, $5F, $00, $7E, $05, $A8, $37, $FE, $02, $24, $8B, $34, $30, $3E, $0C
.byte $4E, $43, $AE, $0A, $BE, $0C, $EE, $0A, $FE, $0C, $2E, $8A, $3E, $0C, $7E, $02
.byte $8E, $0E, $98, $36, $B9, $34, $08, $BF, $09, $3F, $0E, $82, $2E, $86, $4E, $0C
.byte $9E, $09, $C1, $62, $C4, $0B, $EE, $0C, $0E, $86, $5E, $0C, $7E, $09, $A1, $62
.byte $A4, $0B, $CE, $0C, $FE, $0A, $28, $B4, $A6, $31, $E8, $34, $8B, $B2, $9B, $0B
.byte $FE, $07, $FE, $8A, $0D, $C4, $CD, $43, $CE, $09, $DD, $42, $DE, $0B, $FE, $02
.byte $5D, $C7, $FD

L_HArea03:
.byte $5B, $03, $05, $34, $06, $35, $39, $71, $6E, $02, $AE, $0A, $FE, $05, $17, $8B
.byte $97, $0B, $9E, $02, $A6, $04, $FA, $30, $FE, $0A, $4E, $82, $57, $0B, $58, $62
.byte $68, $62, $79, $61, $8A, $60, $8E, $0A, $F5, $31, $F9, $73, $39, $F3, $B5, $71
.byte $B7, $31, $4D, $C8, $8A, $62, $9A, $62, $AE, $05, $BB, $0B, $CD, $4A, $FE, $82
.byte $77, $FB, $DE, $0F, $4E, $82, $6D, $47, $39, $F3, $0C, $EA, $08, $3F, $B3, $00
.byte $CC, $63, $F9, $30, $69, $F9, $EA, $60, $F9, $61, $FE, $07, $DE, $84, $E4, $62
.byte $E9, $61, $F4, $62, $FA, $60, $04, $E2, $14, $62, $24, $62, $34, $62, $3E, $0A
.byte $7E, $0C, $7E, $8A, $8E, $08, $94, $36, $FE, $0A, $0D, $C4, $61, $64, $71, $64
.byte $81, $64, $CD, $43, $CE, $09, $DD, $42, $DE, $0B, $FE, $02, $5D, $C7, $FD

L_HArea04:
.byte $52, $71, $0F, $20, $6E, $70, $E3, $64, $FC, $61, $FC, $71, $13, $84, $2C, $61
.byte $2C, $71, $43, $64, $B2, $22, $B5, $62, $C7, $28, $22, $A2, $52, $06, $56, $61
.byte $6C, $03, $DB, $71, $FC, $03, $F3, $20, $03, $A4, $0F, $71, $40, $09, $86, $47
.byte $8C, $74, $9C, $66, $D7, $00, $EC, $71, $89, $E1, $B6, $61, $B9, $2A, $C7, $26
.byte $F4, $23, $67, $E2, $E8, $F2, $7C, $F4, $03, $A6, $07, $26, $21, $79, $4B, $71
.byte $CF, $33, $06, $E4, $16, $2A, $39, $71, $58, $45, $5A, $45, $C6, $05, $DC, $04
.byte $3F, $E7, $3B, $71, $8C, $71, $AC, $01, $E7, $63, $39, $8C, $63, $20, $65, $08
.byte $68, $62, $8C, $00, $0C, $81, $29, $63, $3C, $01, $57, $65, $6C, $01, $85, $67
.byte $9C, $04, $1D, $C1, $5F, $26, $3D, $C7, $FD

L_HArea05:
.byte $50, $50, $0B, $1E, $0F, $26, $19, $96, $84, $43, $C7, $1E, $6D, $C8, $E3, $12
.byte $39, $9C, $56, $43, $47, $9A, $A4, $12, $C1, $04, $F4, $42, $1B, $98, $A7, $14
.byte $02, $C2, $03, $12, $57, $1E, $AD, $48, $63, $9C, $82, $48, $86, $92, $08, $94
.byte $8E, $11, $B0, $02, $C9, $0C, $1D, $C1, $2D, $4A, $4E, $42, $6F, $20, $0D, $0E
.byte $0E, $40, $39, $71, $7F, $37, $F2, $68, $01, $E9, $11, $39, $68, $7A, $DE, $1F
.byte $6D, $C5, $FD

L_HArea06:
.byte $52, $B1, $0F, $20, $6E, $75, $CC, $73, $A3, $B3, $BF, $74, $0C, $84, $83, $3F
.byte $9F, $74, $EF, $71, $EC, $01, $2F, $F1, $2C, $01, $6F, $71, $A8, $91, $AA, $10
.byte $77, $FB, $56, $F4, $39, $F1, $BF, $37, $33, $E7, $43, $03, $47, $02, $6C, $05
.byte $C3, $67, $D3, $67, $E3, $67, $FC, $07, $73, $E7, $83, $67, $93, $67, $A3, $67
.byte $BC, $08, $43, $E7, $53, $67, $DC, $02, $59, $91, $C3, $33, $D9, $71, $DF, $72
.byte $5B, $F1, $9B, $71, $3B, $F1, $A7, $C2, $DB, $71, $0D, $10, $9B, $71, $0A, $B0
.byte $1C, $04, $67, $63, $76, $64, $85, $65, $94, $66, $A3, $67, $B3, $67, $CC, $09
.byte $73, $A3, $87, $22, $B3, $06, $D6, $82, $E3, $02, $FE, $3F, $0D, $15, $DE, $31
.byte $EC, $01, $03, $F7, $9D, $41, $DF, $26, $BD, $C7, $FD

L_HArea07:
.byte $55, $10, $0B, $1F, $0F, $26, $D6, $12, $07, $9F, $33, $1A, $FB, $1F, $F7, $94
.byte $24, $88, $53, $14, $71, $71, $CC, $15, $CF, $13, $1F, $98, $63, $12, $9B, $13
.byte $A9, $71, $FB, $17, $09, $F1, $13, $13, $21, $42, $59, $0C, $EB, $13, $33, $93
.byte $40, $04, $8C, $14, $8F, $17, $93, $40, $CF, $13, $0B, $94, $57, $15, $07, $93
.byte $24, $08, $19, $F3, $C6, $43, $C7, $13, $D3, $02, $E3, $02, $33, $B0, $4A, $72
.byte $55, $46, $73, $31, $A8, $74, $E3, $12, $8E, $91, $AD, $41, $CE, $42, $EF, $20
.byte $DD, $C7, $FD

L_HArea08:
.byte $52, $31, $0F, $20, $6E, $74, $0D, $02, $03, $33, $1F, $72, $39, $71, $65, $03
.byte $6C, $70, $77, $00, $84, $72, $8C, $72, $B3, $34, $EC, $01, $EF, $72, $0D, $04
.byte $AC, $67, $CC, $01, $CF, $71, $E7, $2B, $23, $80, $3C, $62, $65, $71, $67, $33
.byte $8C, $61, $DC, $01, $08, $FA, $45, $75, $63, $07, $73, $23, $7C, $02, $8F, $72
.byte $73, $A9, $9F, $74, $BF, $74, $EF, $73, $39, $F1, $FC, $0A, $0D, $0B, $13, $25
.byte $4C, $01, $4F, $72, $73, $08, $77, $02, $DC, $08, $23, $A2, $53, $06, $56, $02
.byte $63, $24, $8C, $02, $3F, $B3, $77, $63, $96, $74, $B3, $77, $5D, $C1, $8F, $26
.byte $7D, $C7, $FD

L_HArea09:
.byte $54, $11, $0F, $26, $CF, $32, $F8, $62, $FE, $10, $3C, $B2, $BD, $48, $EA, $62
.byte $FC, $4D, $FC, $4D, $17, $C9, $DA, $62, $0B, $97, $B7, $12, $2C, $B1, $33, $43
.byte $6C, $31, $AC, $41, $0B, $98, $AD, $4A, $DB, $30, $27, $B0, $B7, $14, $C6, $42
.byte $C7, $96, $D6, $44, $2B, $92, $39, $0C, $72, $41, $A7, $00, $1B, $95, $97, $13
.byte $6C, $95, $6F, $11, $A2, $40, $BF, $15, $C2, $40, $0B, $9A, $62, $42, $63, $12
.byte $AD, $4A, $0E, $91, $1D, $41, $4F, $26, $4D, $C7, $FD

L_HArea0A:
.byte $57, $11, $0F, $26, $FE, $10, $4B, $92, $59, $0C, $D3, $93, $0B, $94, $29, $0C
.byte $7B, $93, $99, $0C, $0D, $06, $27, $12, $35, $0C, $23, $B1, $57, $75, $A3, $31
.byte $AB, $71, $F7, $75, $23, $B1, $87, $13, $95, $0C, $0D, $0A, $23, $35, $38, $13
.byte $55, $00, $9B, $16, $0B, $96, $C7, $75, $3B, $92, $49, $0C, $29, $92, $52, $40
.byte $6C, $15, $6F, $11, $72, $40, $BF, $15, $02, $C3, $03, $13, $0A, $13, $8B, $12
.byte $99, $0C, $0D, $10, $47, $16, $46, $45, $94, $08, $B3, $32, $13, $B1, $57, $0B
.byte $A7, $0B, $D3, $31, $53, $B1, $A6, $31, $03, $B2, $13, $0B, $AE, $11, $BD, $41
.byte $EE, $52, $0F, $A0, $DD, $47, $FD

L_HArea0B:
.byte $52, $A1, $0F, $20, $6E, $65, $39, $F1, $60, $21, $6F, $62, $AC, $75, $07, $80
.byte $1C, $78, $B0, $33, $CF, $66, $57, $E3, $6C, $04, $9A, $B0, $AC, $0C, $83, $B1
.byte $8F, $74, $F8, $11, $FA, $10, $83, $85, $93, $22, $9F, $74, $59, $F9, $89, $61
.byte $A9, $61, $BC, $0C, $67, $A0, $EB, $71, $77, $85, $7A, $10, $86, $51, $95, $52
.byte $A4, $53, $B6, $03, $B3, $06, $D3, $23, $26, $84, $4A, $10, $53, $23, $5C, $00
.byte $6F, $73, $93, $05, $07, $F3, $2C, $04, $33, $30, $74, $76, $EB, $71, $57, $88
.byte $6C, $02, $96, $74, $E3, $30, $0C, $86, $7D, $41, $BF, $26, $BD, $C7, $FD

L_HArea0C:
.byte $55, $A1, $0F, $26, $9C, $01, $4F, $B6, $B3, $34, $C9, $3F, $13, $BA, $A3, $B3
.byte $BF, $74, $0C, $84, $83, $3F, $9F, $74, $EF, $72, $EC, $01, $2F, $F2, $2C, $01
.byte $6F, $72, $6C, $01, $A8, $91, $AA, $10, $03, $B7, $10, $08, $61, $79, $6F, $75
.byte $39, $F1, $DB, $71, $03, $A2, $17, $22, $33, $06, $43, $20, $5B, $71, $48, $8C
.byte $4A, $30, $5C, $5C, $93, $31, $2D, $C1, $5F, $26, $3D, $C7, $FD

L_HArea0D:
.byte $55, $A1, $0F, $26, $39, $91, $68, $12, $A7, $12, $AA, $10, $C7, $05, $E8, $12
.byte $19, $91, $6C, $00, $78, $74, $0E, $C2, $76, $A8, $FE, $40, $29, $91, $73, $29
.byte $77, $53, $86, $47, $8C, $76, $F7, $00, $59, $91, $87, $13, $B6, $14, $BA, $10
.byte $E8, $12, $38, $92, $19, $8C, $2C, $00, $33, $67, $4E, $42, $68, $08, $2E, $C0
.byte $38, $72, $A8, $11, $AA, $10, $49, $91, $6E, $42, $DE, $40, $E7, $22, $0E, $C2
.byte $4E, $C0, $6C, $00, $79, $11, $8C, $01, $A7, $13, $BC, $01, $D5, $15, $EC, $01
.byte $03, $97, $0E, $00, $6E, $01, $9D, $41, $CE, $42, $FF, $20, $9D, $C7, $FD

L_HArea0E:
.byte $10, $21, $39, $F1, $09, $F1, $A8, $60, $7C, $83, $96, $30, $5B, $F1, $C8, $04
.byte $1F, $B7, $93, $67, $A3, $67, $B3, $67, $B8, $60, $CC, $08, $54, $FE, $6E, $2F
.byte $6D, $C7, $FD

L_HArea0F:
.byte $00, $C1, $4C, $00, $02, $C9, $BA, $49, $62, $C9, $A4, $20, $A5, $20, $1A, $C9
.byte $A3, $2C, $B2, $49, $56, $C2, $6E, $00, $95, $41, $AD, $C7, $FD

L_HArea12:
.byte $48, $8F, $1E, $01, $4E, $02, $00, $89, $09, $0C, $6E, $0A, $EE, $82, $2E, $80
.byte $30, $20, $7E, $01, $87, $27, $07, $85, $17, $23, $3E, $00, $9E, $05, $5B, $F1
.byte $8B, $71, $BB, $71, $EB, $71, $3E, $82, $7F, $38, $FE, $0A, $3E, $84, $47, $29
.byte $48, $2E, $AF, $71, $CB, $71, $E7, $07, $F7, $23, $2B, $F1, $37, $51, $3E, $00
.byte $6F, $00, $8E, $04, $DF, $32, $9C, $82, $CA, $12, $DC, $00, $E8, $14, $FC, $00
.byte $FE, $08, $4E, $8A, $88, $74, $9E, $01, $A8, $52, $BF, $47, $B8, $52, $C8, $52
.byte $D8, $52, $E8, $52, $EE, $0F, $4D, $C7, $0D, $0D, $0E, $02, $68, $7A, $BE, $01
.byte $EE, $0F, $6D, $C5, $FD

L_HArea13:
.byte $08, $0F, $0E, $01, $2E, $05, $38, $20, $3E, $04, $48, $05, $55, $45, $57, $45
.byte $58, $25, $B8, $05, $BE, $05, $C8, $20, $CE, $01, $DF, $4A, $6D, $C7, $0E, $81
.byte $00, $5A, $2E, $02, $34, $42, $36, $42, $37, $22, $73, $54, $83, $08, $87, $20
.byte $93, $54, $90, $05, $B4, $41, $B6, $41, $B7, $21, $DF, $4A, $6D, $C7, $0E, $81
.byte $00, $5A, $14, $56, $24, $56, $2E, $0C, $33, $43, $6E, $09, $8E, $0B, $96, $48
.byte $1E, $84, $3E, $05, $4A, $48, $47, $08, $CE, $01, $DF, $4A, $6D, $C7, $FD

L_HArea14:
.byte $41, $01, $DA, $60, $E9, $61, $F8, $62, $FE, $0B, $FE, $81, $47, $D3, $8A, $60
.byte $99, $61, $A8, $62, $B7, $63, $C6, $64, $D5, $65, $E4, $66, $ED, $49, $F3, $67
.byte $1A, $CB, $E3, $67, $F3, $67, $FE, $02, $31, $D6, $3C, $02, $77, $53, $AC, $02
.byte $B1, $56, $E7, $53, $FE, $01, $77, $B9, $A3, $43, $00, $BF, $29, $51, $39, $48
.byte $61, $55, $D2, $44, $D6, $54, $0C, $82, $2E, $02, $31, $66, $44, $47, $47, $32
.byte $4A, $47, $97, $32, $C1, $66, $CE, $01, $DC, $02, $FE, $0E, $0C, $8F, $08, $4F
.byte $FE, $02, $75, $E0, $FE, $01, $0C, $87, $9A, $60, $A9, $61, $B8, $62, $C7, $63
.byte $CE, $0F, $D5, $0A, $6D, $CA, $7D, $47, $FD

;unused bytes
.byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
.byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
.byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
.byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
.byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
.else
;--------------------------------------------------

;level A-4
E_CastleArea11:
  .byte $2a, $9e, $6b, $0c, $8d, $1c, $ea, $1f, $1b, $8c, $e6, $1c, $8c, $9c, $bb, $0c
  .byte $f3, $83, $9b, $8c, $db, $0c, $1b, $8c, $6b, $0c, $bb, $0c, $0f, $09, $40, $15
  .byte $78, $ad, $90, $b5, $ff

;level B-4
E_CastleArea12:
  .byte $0f, $02, $38, $1d, $d9, $1b, $6e, $e1, $21, $3a, $a8, $18, $9d, $0f, $07, $18
  .byte $1d, $0f, $09, $18, $1d, $0f, $0b, $18, $1d, $7b, $15, $8e, $21, $2e, $b9, $9d
  .byte $0f, $0e, $78, $2d, $90, $b5, $ff

;level C-4
E_CastleArea13:
  .byte $05, $9d, $65, $1d, $0d, $a8, $dd, $1d, $07, $ac, $54, $2c, $a2, $2c, $f4, $2c
  .byte $42, $ac, $26, $9d, $d4, $03, $24, $83, $64, $03, $2b, $82, $4b, $02, $7b, $02
  .byte $9b, $02, $5b, $82, $7b, $02, $0b, $82, $2b, $02, $c6, $1b, $28, $82, $48, $02
  .byte $a6, $1b, $7b, $95, $85, $0c, $9d, $9b, $0f, $0e, $78, $2d, $7a, $1d, $90, $b5
  .byte $ff

;level D-4
E_CastleArea14:
  .byte $19, $9f, $99, $1b, $2c, $8c, $59, $1b, $c5, $0f, $0f, $04, $09, $29, $bd, $1d
  .byte $0f, $06, $6e, $2a, $61, $0f, $09, $48, $2d, $46, $87, $79, $07, $8e, $63, $60
  .byte $a5, $07, $b8, $85, $57, $a5, $8c, $8c, $76, $9d, $78, $2d, $90, $b5, $ff

;level A-1
E_GroundArea30:
  .byte $07, $83, $37, $03, $6b, $0e, $e0, $3d, $20, $be, $6e, $2b, $00, $a7, $85, $d3
  .byte $05, $e7, $83, $24, $83, $27, $03, $49, $00, $59, $00, $10, $bb, $b0, $3b, $6e
  .byte $c1, $00, $17, $85, $53, $05, $36, $8e, $76, $0e, $b6, $0e, $e7, $83, $63, $83
  .byte $68, $03, $29, $83, $57, $03, $85, $03, $b5, $29, $ff

;level A-3
E_GroundArea31:
  .byte $0f, $04, $66, $07, $0f, $06, $86, $10, $0f, $08, $55, $0f, $e5, $8f, $ff

;level B-1
E_GroundArea32:
  .byte $70, $b7, $ca, $00, $66, $80, $0f, $04, $79, $0e, $ab, $0e, $ee, $2b, $20, $eb
  .byte $80, $40, $bb, $fb, $00, $40, $b7, $cb, $0e, $0f, $09, $4b, $00, $76, $00, $d8
  .byte $00, $6b, $8e, $73, $06, $83, $06, $c7, $0e, $36, $90, $c5, $06, $ff

;level B-3
E_GroundArea33:
  .byte $84, $8f, $a7, $24, $d3, $0f, $ea, $24, $45, $a9, $d5, $28, $45, $a9, $84, $25
  .byte $b4, $8f, $09, $90, $b5, $a8, $5b, $97, $cd, $28, $b5, $a4, $09, $a4, $65, $28
  .byte $92, $90, $e3, $83, $ff

;level C-1
E_GroundArea34:
  .byte $3a, $8e, $5b, $0e, $c3, $8e, $ca, $8e, $0b, $8e, $4a, $0e, $de, $c1, $44, $0f
  .byte $08, $49, $0e, $eb, $0e, $8a, $90, $ab, $85, $0f, $0c, $03, $0f, $2e, $2b, $40
  .byte $67, $86, $ff

;level C-2
E_GroundArea35:
  .byte $15, $8f, $54, $07, $aa, $83, $f8, $07, $0f, $04, $14, $07, $96, $10, $0f, $07
  .byte $95, $0f, $9d, $a8, $0b, $97, $09, $a9, $55, $24, $a9, $24, $bb, $17, $ff

;level C-3
E_GroundArea36:
  .byte $0f, $03, $a6, $11, $a3, $90, $a6, $91, $0f, $08, $a6, $11, $e3, $a9, $0f, $0d
  .byte $55, $24, $a9, $24, $0f, $11, $59, $1d, $a9, $1b, $23, $8f, $15, $9b, $ff

;level D-1
E_GroundArea37:
  .byte $87, $85, $9b, $05, $18, $90, $a4, $8f, $6e, $c1, $60, $9b, $02, $d0, $3b, $80
  .byte $b8, $03, $8e, $1b, $02, $3b, $02, $0f, $08, $03, $10, $f7, $05, $6b, $85, $ff

;level D-2
E_GroundArea38:
  .byte $db, $82, $f3, $03, $10, $b7, $80, $37, $1a, $8e, $4b, $0e, $7a, $0e, $ab, $0e
  .byte $0f, $05, $f9, $0e, $d0, $be, $2e, $c1, $62, $d4, $8f, $64, $8f, $7e, $2b, $60
  .byte $ff

;level D-3
E_GroundArea39:
  .byte $0f, $03, $ab, $05, $1b, $85, $a3, $85, $d7, $05, $0f, $08, $33, $03, $0b, $85
  .byte $fb, $85, $8b, $85, $3a, $8e, $ff

;ground level area used with level D-4
E_GroundArea40:
  .byte $0f, $02, $09, $05, $3e, $41, $64, $2b, $8e, $58, $0e, $ca, $07, $34, $87, $ff

;cloud level used with levels A-1, B-1 and D-2
E_GroundArea41:
  .byte $0a, $aa, $1e, $20, $03, $1e, $22, $27, $2e, $24, $48, $2e, $28, $67, $ff

;level A-2
E_UndergroundArea6:
  .byte $bb, $a9, $1b, $a9, $69, $29, $b8, $29, $59, $a9, $8d, $a8, $0f, $07, $15, $29
  .byte $55, $ac, $6b, $85, $0e, $ad, $01, $67, $34, $ff

;underground bonus rooms used with worlds A-D
E_UndergroundArea7:
  .byte $1e, $a0, $09, $1e, $27, $67, $0f, $03, $1e, $28, $68, $0f, $05, $1e, $24, $48
  .byte $1e, $63, $68, $ff

;level B-2
E_WaterArea9:
  .byte $ee, $ad, $21, $26, $87, $f3, $0e, $66, $87, $cb, $00, $65, $87, $0f, $06, $06
  .byte $0e, $97, $07, $cb, $00, $75, $87, $d3, $27, $d9, $27, $0f, $09, $77, $1f, $46
  .byte $87, $b1, $0f, $ff

;level A-4
L_CastleArea11:
  .byte $9b, $87, $05, $32, $06, $33, $07, $34, $ee, $0a, $0e, $86, $28, $0e, $3e, $0a
  .byte $6e, $02, $8b, $0e, $97, $00, $9e, $0a, $ce, $06, $e8, $0e, $fe, $0a, $2e, $86
  .byte $6e, $0a, $8e, $08, $e4, $0e, $1e, $82, $8a, $0e, $8e, $0a, $fe, $02, $1a, $e0
  .byte $29, $61, $2e, $06, $3e, $09, $56, $60, $65, $61, $6e, $0c, $83, $60, $7e, $8a
  .byte $bb, $61, $f9, $63, $27, $e5, $88, $64, $eb, $61, $fe, $05, $68, $90, $0a, $90
  .byte $fe, $02, $3a, $90, $3e, $0a, $ae, $02, $da, $60, $e9, $61, $f8, $62, $fe, $0a
  .byte $0d, $c4, $a1, $62, $b1, $62, $cd, $43, $ce, $09, $de, $0b, $dd, $42, $fe, $02
  .byte $5d, $c7, $fd

;level B-4
L_CastleArea12:
  .byte $9b, $07, $05, $32, $06, $33, $07, $33, $3e, $0a, $41, $3b, $42, $3b, $58, $64
  .byte $7a, $62, $c8, $31, $18, $e4, $39, $73, $5e, $09, $66, $3c, $0e, $82, $28, $07
  .byte $36, $0e, $3e, $0a, $ae, $02, $d7, $0e, $fe, $0c, $fe, $8a, $11, $e5, $21, $65
  .byte $31, $65, $4e, $0c, $fe, $02, $16, $8e, $2e, $0e, $fe, $02, $18, $fa, $3e, $0e
  .byte $fe, $02, $16, $8e, $2e, $0e, $fe, $02, $18, $fa, $3e, $0e, $fe, $02, $16, $8e
  .byte $2e, $0e, $fe, $02, $18, $fa, $3e, $0e, $fe, $02, $16, $8e, $2e, $0e, $fe, $02
  .byte $18, $fa, $5e, $0a, $6e, $02, $7e, $0a, $b7, $0e, $ee, $07, $fe, $8a, $0d, $c4
  .byte $cd, $43, $ce, $09, $dd, $42, $de, $0b, $fe, $02, $5d, $c7, $fd

;level C-4
L_CastleArea13:
  .byte $98, $07, $05, $35, $06, $3d, $07, $3d, $be, $06, $de, $0c, $f3, $3d, $03, $8e
  .byte $63, $0e, $6e, $43, $ce, $0a, $e1, $67, $f1, $67, $01, $e7, $11, $67, $1e, $05
  .byte $28, $39, $6e, $40, $be, $01, $c7, $06, $db, $0e, $de, $00, $1f, $80, $6f, $00
  .byte $bf, $00, $0f, $80, $5f, $00, $7e, $05, $a8, $37, $fe, $02, $24, $8e, $34, $30
  .byte $3e, $0c, $4e, $43, $ae, $0a, $be, $0c, $ee, $0a, $fe, $0c, $2e, $8a, $3e, $0c
  .byte $7e, $02, $8e, $0e, $98, $36, $b9, $34, $08, $bf, $09, $3f, $0e, $82, $2e, $86
  .byte $4e, $0c, $9e, $09, $a6, $60, $c1, $62, $c4, $0e, $ee, $0c, $0e, $86, $5e, $0c
  .byte $7e, $09, $86, $60, $a1, $62, $a4, $0e, $c6, $60, $ce, $0c, $fe, $0a, $28, $b4
  .byte $a6, $31, $e8, $34, $8b, $b2, $9b, $0e, $fe, $07, $fe, $8a, $0d, $c4, $cd, $43
  .byte $ce, $09, $dd, $42, $de, $0b, $fe, $02, $5d, $c7, $fd

;level D-4
L_CastleArea14:
  .byte $5b, $03, $05, $34, $06, $35, $39, $71, $6e, $02, $ae, $0a, $fe, $05, $17, $8e
  .byte $97, $0e, $9e, $02, $a6, $06, $fa, $30, $fe, $0a, $4e, $82, $57, $0e, $58, $62
  .byte $68, $62, $79, $61, $8a, $60, $8e, $0a, $f5, $31, $f9, $73, $39, $f3, $b5, $71
  .byte $b7, $31, $4d, $c8, $8a, $62, $9a, $62, $ae, $05, $bb, $0e, $cd, $4a, $fe, $82
  .byte $77, $fb, $de, $0f, $4e, $82, $6d, $47, $39, $f3, $0c, $ea, $08, $3f, $b3, $00
  .byte $cc, $63, $f9, $30, $69, $f9, $ea, $60, $f9, $61, $fe, $07, $de, $84, $e4, $62
  .byte $e9, $61, $f4, $62, $fa, $60, $04, $e2, $14, $62, $24, $62, $34, $62, $3e, $0a
  .byte $7e, $0c, $7e, $8a, $8e, $08, $94, $36, $fe, $0a, $0d, $c4, $61, $64, $71, $64
  .byte $81, $64, $cd, $43, $ce, $09, $dd, $42, $de, $0b, $fe, $02, $5d, $c7, $fd

;level A-1
L_GroundArea30:
  .byte $52, $71, $0f, $20, $6e, $70, $e3, $64, $fc, $61, $fc, $71, $13, $86, $2c, $61
  .byte $2c, $71, $43, $64, $b2, $22, $b5, $62, $c7, $28, $22, $a2, $52, $09, $56, $61
  .byte $6c, $03, $db, $71, $fc, $03, $f3, $20, $03, $a4, $0f, $71, $40, $0c, $8c, $74
  .byte $9c, $66, $d7, $01, $ec, $71, $89, $e1, $b6, $61, $b9, $2a, $c7, $26, $f4, $23
  .byte $67, $e2, $e8, $f2, $78, $82, $88, $01, $98, $02, $a8, $02, $b8, $02, $03, $a6
  .byte $07, $26, $21, $79, $4b, $71, $cf, $33, $06, $e4, $16, $2a, $39, $71, $58, $45
  .byte $5a, $45, $c6, $07, $dc, $04, $3f, $e7, $3b, $71, $8c, $71, $ac, $01, $e7, $63
  .byte $39, $8f, $63, $20, $65, $0b, $68, $62, $8c, $00, $0c, $81, $29, $63, $3c, $01
  .byte $57, $65, $6c, $01, $85, $67, $9c, $04, $1d, $c1, $5f, $26, $3d, $c7, $fd

;level A-3
L_GroundArea31:
  .byte $50, $50, $0b, $1f, $0f, $26, $19, $96, $84, $43, $b7, $1f, $5d, $cc, $6d, $48
  .byte $e0, $42, $e3, $12, $39, $9c, $56, $43, $47, $9b, $a4, $12, $c1, $06, $ed, $4d
  .byte $f4, $42, $1b, $98, $b7, $13, $02, $c2, $03, $12, $47, $1f, $ad, $48, $63, $9c
  .byte $82, $48, $76, $93, $08, $94, $8e, $11, $b0, $03, $c9, $0f, $1d, $c1, $2d, $4a
  .byte $4e, $42, $6f, $20, $0d, $0e, $0e, $40, $39, $71, $7f, $37, $f2, $68, $01, $e9
  .byte $11, $39, $68, $7a, $de, $1f, $6d, $c5, $fd

;level B-1
L_GroundArea32:
  .byte $52, $21, $0f, $20, $6e, $60, $6c, $f6, $ca, $30, $dc, $02, $08, $f2, $37, $04
  .byte $56, $74, $7c, $00, $dc, $01, $e7, $25, $47, $8b, $49, $20, $6c, $02, $96, $74
  .byte $06, $82, $36, $02, $66, $00, $a7, $22, $dc, $02, $0a, $e0, $63, $22, $78, $72
  .byte $93, $09, $97, $03, $a3, $25, $a7, $03, $b6, $24, $03, $a2, $5c, $75, $65, $71
  .byte $7c, $00, $9c, $00, $63, $a2, $67, $20, $77, $03, $87, $20, $93, $0a, $97, $03
  .byte $a3, $22, $a7, $20, $b7, $03, $bc, $00, $c7, $20, $dc, $00, $fc, $01, $19, $8f
  .byte $1e, $20, $46, $22, $4c, $61, $63, $00, $8e, $21, $d7, $73, $46, $a6, $4c, $62
  .byte $68, $62, $73, $01, $8c, $62, $d8, $62, $43, $a9, $c7, $73, $ec, $06, $57, $f3
  .byte $7c, $00, $b5, $65, $c5, $65, $dc, $00, $e3, $67, $7d, $c1, $bf, $26, $ad, $c7
  .byte $fd

;level B-3
L_GroundArea33:
  .byte $90, $10, $0b, $1b, $0f, $26, $07, $94, $bc, $14, $bf, $13, $c7, $40, $ff, $16
  .byte $d1, $80, $c3, $94, $cb, $17, $c2, $44, $29, $8f, $77, $31, $0b, $96, $76, $32
  .byte $c7, $75, $13, $f7, $1b, $61, $2b, $61, $4b, $12, $59, $0f, $3b, $b0, $3a, $40
  .byte $43, $12, $7a, $40, $7b, $30, $b5, $41, $b6, $20, $c6, $07, $f3, $13, $03, $92
  .byte $6b, $12, $79, $0f, $cc, $15, $cf, $11, $1f, $95, $c3, $14, $b3, $95, $a3, $95
  .byte $4d, $ca, $6b, $61, $7e, $11, $8d, $41, $be, $42, $df, $20, $bd, $c7, $fd

;level C-1
L_GroundArea34:
  .byte $52, $31, $0f, $20, $6e, $74, $0d, $02, $03, $33, $1f, $72, $39, $71, $65, $04
  .byte $6c, $70, $77, $01, $84, $72, $8c, $72, $b3, $34, $ec, $01, $ef, $72, $0d, $04
  .byte $ac, $67, $cc, $01, $cf, $71, $e7, $22, $17, $88, $23, $00, $27, $23, $3c, $62
  .byte $65, $71, $67, $33, $8c, $61, $dc, $01, $08, $fa, $45, $75, $63, $0a, $73, $23
  .byte $7c, $02, $8f, $72, $73, $a9, $9f, $74, $bf, $74, $ef, $73, $39, $f1, $fc, $0a
  .byte $0d, $0b, $13, $25, $4c, $01, $4f, $72, $73, $0b, $77, $03, $dc, $08, $23, $a2
  .byte $53, $09, $56, $03, $63, $24, $8c, $02, $3f, $b3, $77, $63, $96, $74, $b3, $77
  .byte $5d, $c1, $8f, $26, $7d, $c7, $fd

;level C-2
L_GroundArea35:
  .byte $54, $11, $0f, $26, $cf, $32, $f8, $62, $fe, $10, $3c, $b2, $bd, $48, $ea, $62
  .byte $fc, $4d, $fc, $4d, $17, $c9, $da, $62, $0b, $97, $b7, $12, $2c, $b1, $33, $43
  .byte $6c, $31, $ac, $41, $0b, $98, $ad, $4a, $db, $30, $27, $b0, $b7, $14, $c6, $42
  .byte $c7, $96, $d6, $44, $2b, $92, $39, $0f, $72, $41, $a7, $00, $1b, $95, $97, $13
  .byte $6c, $95, $6f, $11, $a2, $40, $bf, $15, $c2, $40, $0b, $9a, $62, $42, $63, $12
  .byte $ad, $4a, $0e, $91, $1d, $41, $4f, $26, $4d, $c7, $fd

;level C-3
L_GroundArea36:
  .byte $57, $11, $0f, $26, $fe, $10, $4b, $92, $59, $0f, $ad, $4c, $d3, $93, $0b, $94
  .byte $29, $0f, $7b, $93, $99, $0f, $0d, $06, $27, $12, $35, $0f, $23, $b1, $57, $75
  .byte $a3, $31, $ab, $71, $f7, $75, $23, $b1, $87, $13, $95, $0f, $0d, $0a, $23, $35
  .byte $38, $13, $55, $00, $9b, $16, $0b, $96, $c7, $75, $dd, $4a, $3b, $92, $49, $0f
  .byte $ad, $4c, $29, $92, $52, $40, $6c, $15, $6f, $11, $72, $40, $bf, $15, $03, $93
  .byte $0a, $13, $12, $41, $8b, $12, $99, $0f, $0d, $10, $47, $16, $46, $45, $b3, $32
  .byte $13, $b1, $57, $0e, $a7, $0e, $d3, $31, $53, $b1, $a6, $31, $03, $b2, $13, $0e
  .byte $8d, $4d, $ae, $11, $bd, $41, $ee, $52, $0f, $a0, $dd, $47, $fd

;level D-1
L_GroundArea37:
  .byte $52, $a1, $0f, $20, $6e, $65, $04, $a0, $14, $07, $24, $2d, $57, $25, $bc, $09
  .byte $4c, $80, $6f, $33, $a5, $11, $a7, $63, $b7, $63, $e7, $20, $35, $a0, $59, $11
  .byte $b4, $08, $c0, $04, $05, $82, $15, $02, $25, $02, $3a, $10, $4c, $01, $6c, $79
  .byte $95, $79, $73, $a7, $8f, $74, $f3, $0a, $03, $a0, $93, $08, $97, $73, $e3, $20
  .byte $39, $f1, $94, $07, $aa, $30, $bc, $5c, $c7, $30, $24, $f2, $27, $31, $8f, $33
  .byte $c6, $10, $c7, $63, $d7, $63, $e7, $63, $f7, $63, $03, $a5, $07, $25, $aa, $10
  .byte $03, $bf, $4f, $74, $6c, $00, $df, $74, $fc, $00, $5c, $81, $77, $73, $9d, $4c
  .byte $c5, $30, $e3, $30, $7d, $c1, $bd, $4d, $bf, $26, $ad, $c7, $fd

;level D-2
L_GroundArea38:
  .byte $55, $a1, $0f, $26, $9c, $01, $4f, $b6, $b3, $34, $c9, $3f, $13, $ba, $a3, $b3
  .byte $bf, $74, $0c, $84, $83, $3f, $9f, $74, $ef, $72, $ec, $01, $2f, $f2, $2c, $01
  .byte $6f, $72, $6c, $01, $a8, $91, $aa, $10, $03, $b7, $61, $79, $6f, $75, $39, $f1
  .byte $db, $71, $03, $a2, $17, $22, $33, $09, $43, $20, $5b, $71, $48, $8f, $4a, $30
  .byte $5c, $5c, $a3, $30, $2d, $c1, $5f, $26, $3d, $c7, $fd

;level D-3
L_GroundArea39:
  .byte $55, $a1, $0f, $26, $39, $91, $68, $12, $a7, $12, $aa, $10, $c7, $07, $e8, $12
  .byte $19, $91, $6c, $00, $78, $74, $0e, $c2, $76, $a8, $fe, $40, $29, $91, $73, $29
  .byte $77, $53, $8c, $77, $59, $91, $87, $13, $b6, $14, $ba, $10, $e8, $12, $38, $92
  .byte $19, $8f, $2c, $00, $33, $67, $4e, $42, $68, $0b, $2e, $c0, $38, $72, $a8, $11
  .byte $aa, $10, $49, $91, $6e, $42, $de, $40, $e7, $22, $0e, $c2, $4e, $c0, $6c, $00
  .byte $79, $11, $8c, $01, $a7, $13, $bc, $01, $d5, $15, $ec, $01, $03, $97, $0e, $00
  .byte $6e, $01, $9d, $41, $ce, $42, $ff, $20, $9d, $c7, $fd

;ground level area used with level D-4
L_GroundArea40:
  .byte $10, $21, $39, $f1, $09, $f1, $ad, $4c, $7c, $83, $96, $30, $5b, $f1, $c8, $05
  .byte $1f, $b7, $93, $67, $a3, $67, $b3, $67, $bd, $4d, $cc, $08, $54, $fe, $6e, $2f
  .byte $6d, $c7, $fd

;cloud level used with levels A-1, B-1 and D-2
L_GroundArea41:
  .byte $00, $c1, $4c, $00, $02, $c9, $ba, $49, $62, $c9, $a4, $20, $a5, $20, $1a, $c9
  .byte $a3, $2c, $b2, $49, $56, $c2, $6e, $00, $95, $41, $ad, $c7, $fd

;level A-2
L_UndergroundArea6:
  .byte $48, $8f, $1e, $01, $4e, $02, $00, $8c, $09, $0f, $6e, $0a, $ee, $82, $2e, $80
  .byte $30, $20, $7e, $01, $87, $27, $07, $87, $17, $23, $3e, $00, $9e, $05, $5b, $f1
  .byte $8b, $71, $bb, $71, $eb, $71, $3e, $82, $7f, $38, $fe, $0a, $3e, $84, $47, $29
  .byte $48, $2e, $af, $71, $cb, $71, $e7, $0a, $f7, $23, $2b, $f1, $37, $51, $3e, $00
  .byte $6f, $00, $8e, $04, $df, $32, $9c, $82, $ca, $12, $dc, $00, $e8, $14, $fc, $00
  .byte $fe, $08, $4e, $8a, $88, $74, $9e, $01, $a8, $52, $bf, $47, $b8, $52, $c8, $52
  .byte $d8, $52, $e8, $52, $ee, $0f, $4d, $c7, $0d, $0d, $0e, $02, $68, $7a, $be, $01
  .byte $ee, $0f, $6d, $c5, $fd

;underground bonus rooms used with worlds A-D
L_UndergroundArea7:
  .byte $08, $0f, $0e, $01, $2e, $05, $38, $20, $3e, $04, $48, $07, $55, $45, $57, $45
  .byte $58, $25, $b8, $08, $be, $05, $c8, $20, $ce, $01, $df, $4a, $6d, $c7, $0e, $81
  .byte $00, $5a, $2e, $02, $34, $42, $36, $42, $37, $22, $73, $54, $83, $0b, $87, $20
  .byte $93, $54, $90, $07, $b4, $41, $b6, $41, $b7, $21, $df, $4a, $6d, $c7, $0e, $81
  .byte $00, $5a, $14, $56, $24, $56, $2e, $0c, $33, $43, $6e, $09, $8e, $0b, $96, $48
  .byte $1e, $84, $3e, $05, $4a, $48, $47, $0b, $ce, $01, $df, $4a, $6d, $c7, $fd

;level B-2
L_WaterArea9:
  .byte $41, $01, $da, $60, $e9, $61, $f8, $62, $fe, $0b, $fe, $81, $47, $d3, $8a, $60
  .byte $99, $61, $a8, $62, $b7, $63, $c6, $64, $d5, $65, $e4, $66, $ed, $49, $f3, $67
  .byte $1a, $cb, $e3, $67, $f3, $67, $fe, $02, $31, $d6, $3c, $02, $77, $53, $ac, $02
  .byte $b1, $56, $e7, $53, $fe, $01, $77, $b9, $a3, $43, $00, $bf, $29, $51, $39, $48
  .byte $61, $55, $d2, $44, $d6, $54, $0c, $82, $2e, $02, $31, $66, $44, $47, $47, $32
  .byte $4a, $47, $97, $32, $c1, $66, $ce, $01, $dc, $02, $fe, $0e, $0c, $8f, $08, $4f
  .byte $fe, $02, $75, $e0, $fe, $01, $0c, $87, $9a, $60, $a9, $61, $b8, $62, $c7, $63
  .byte $ce, $0f, $d5, $0d, $6d, $ca, $7d, $47, $fd

;a bunch of unused space tacked on for no apparent reason
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
  .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
.endif
