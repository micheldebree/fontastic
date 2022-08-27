PROGRAM Fnt2Com;

(* FONTASTIC 1992. Programmed by Michel 'Mad B' de Bree *)

{$I-}

CONST Code : ARRAY [0..25] OF BYTE = ($8A,$3E,$1A,$01,$B3,$00,$B9,$00,$01,
$BA,$00,$00,$8C,$C8,$8E,$C0,$BD,$1B,$01,$B8,$10,$11,$CD,$10,$CD,$20);

VAR input,output : FILE;
    read,written : WORD;
    buffer : ARRAY [0..16*256] OF BYTE;

BEGIN
    WriteLn;
    WriteLn ('FONTASTIC FNT-file to COM-file converter. HeatWave 1992.');
    WriteLn ('--------------------------------------------------------');
    WriteLn;
    IF ParamCount <> 1 THEN
    BEGIN
         WriteLn ('USAGE: FNT2COM <fontname>');
         WriteLn ('       Fontname without extension');
         EXIT;
    END;
    Assign (input,ConCat(ParamStr(1),'.FNT'));
    Assign (output,ConCat(ParamStr(1),'.COM'));
    Reset(input,1);
    IF IOResult <> 0 THEN
    BEGIN
        WriteLn ('Font not found.');
        EXIT;
    END;
    ReWrite (output,1);
    BlockWrite (output,Code,26,written);
    BlockRead (input,buffer,16*256,read);
    BlockWrite (output,buffer,read,written);
    Close(input);
    Close(output);
    WriteLn ('Font succesfully converted...');
END. {FNT2COM}