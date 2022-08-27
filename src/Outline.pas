PROGRAM Outline;

(* FONTASTIC 1992. Programmed by Michel 'Mad B' de Bree. *)

{$I-}

USES Crt;

CONST power : ARRAY [0..7] OF BYTE = (1,2,4,8,16,32,64,128);
      work : ARRAY [0..3] OF CHAR = ('-','\','|','/');

VAR input,output : FILE;
    font : ARRAY [0..16*256] OF BYTE;
    height,i,cx,cy : BYTE;
    size,written : WORD;
    survive : ARRAY [0..7,0..15] OF BOOLEAN;
    wcount : BYTE;

PROCEDURE LoadFont;
BEGIN
    Reset(input,1);
    BlockRead(input,height,1,size);
    BlockRead(input,font,16*256,size);
END; {LoadFont}

PROCEDURE SaveFont;
BEGIN
    ReWrite(output,1);
    BlockWrite (output,height,1,written);
    BlockWrite (output,font,size,written);
END; {SaveFont}

PROCEDURE OutChar (c : BYTE);

VAR ad : WORD;
    x,y : BYTE;

BEGIN
    ad := height*c;
    FOR y := 0 TO height-1 DO
        font[ad+y] := font[ad+y] XOR 255;
    FOR y := 0 TO height-1 DO
    BEGIN
        FOR x := 0 TO 7 DO
        BEGIN
            survive[x,y] := FALSE;
            IF (x>0) AND (font[ad+y] AND power[x-1] = 0) THEN
                survive[x,y] := TRUE;
            IF (x<7) AND (font[ad+y] AND power[x+1] = 0) THEN
                survive[x,y] := TRUE;
            IF (y>0) AND (font[ad+y-1] AND power[x] = 0) THEN
                survive[x,y] := TRUE;
            IF (y<height-1) AND (font[ad+y+1] AND power[x] = 0) THEN
                survive[x,y] := TRUE;
            IF (x>0) AND (y>0) AND (font[ad+y-1] AND power[x-1] = 0) THEN
                survive[x,y] := TRUE;
            IF (x<7) AND (y<height-1) AND (font[ad+y+1] AND power[x+1] = 0) THEN
                survive[x,y] := TRUE;
            IF (x<7) AND (y>0) AND (font[ad+y-1] AND power[x+1] = 0) THEN
                survive[x,y] := TRUE;
            IF (x>0) AND (y<height-1) AND (font[ad+y+1] AND power[x-1] = 0) THEN
                survive[x,y] := TRUE;
        END;
    END;
    FOR y := 0 TO height-1 DO
    BEGIN
        FOR x := 0 TO 7 DO
        BEGIN
        IF NOT survive[x,y] THEN
            font[ad+y] := font[ad+y] AND (power[x] XOR 255);
        END;
     END;
END; {OutChar}

(* M A I N *)

BEGIN
    WriteLn;
    WriteLn ('FONTASTIC Font-Outliner. HeatWave 1992.');
    WriteLn ('---------------------------------------');
    WriteLn;
    IF ParamCount <> 2 THEN
    BEGIN
        WriteLn ('USAGE: OUTLINE <fontname> <new fontname>');
        WriteLn ('       Fontnames without extension');
        EXIT;
    END;
    Assign (input,Concat(ParamStr(1),'.FNT'));
    Assign (output,Concat(ParamStr(2),'.FNT'));
    LoadFont;
    IF IOResult <> 0 THEN
    BEGIN
        WriteLn ('Font not found!');
        EXIT;
    END;
    Write ('Working...');
    cx := WhereX;
    cy := WhereY;
    FOR i := 0 TO 255 DO
    BEGIN
        GotoXY(cx,cy);
        Write(work[wcount]);
        IF (i MOD 5) = 0 THEN
        BEGIN
            INC(wcount);
            wcount := wcount MOD 4;
        END;
        OutChar(i);
    END;
    GotoXY(cx,cy);
    WriteLn ('Done!');
    SaveFont;
    Close (input);
    Close (output);
END. {OutLine}
