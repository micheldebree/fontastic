PROGRAM Fnt2Fnt;

(* FONTASTIC 1992. Programmed by Michel 'Mad B' de Bree. *)

{$I-}

VAR oldh,newh,choise : BYTE;
    Input,Output : File;
    red,written,dummy : WORD;
    buffer : ARRAY [0..255] OF BYTE;

PROCEDURE Expand;
BEGIN
    WriteLn ('Adding ',newh-oldh,' blank lines to the bottom of each char...');
    REPEAT
        BlockRead(input,buffer,oldh,red);
        BlockWrite(output,buffer,red,written);
        FOR dummy := 0 TO newh-oldh DO
        BEGIN
            buffer[dummy] := 0;
        END;
        IF (red = written) AND (red <> 0) THEN
            BlockWrite(output,buffer,newh-oldh,dummy);
    UNTIL (red = 0) OR (red <> written);
END;

PROCEDURE Shrink;

VAR del : ARRAY [1..128] OF BYTE;
    count : WORD;

BEGIN
    WriteLn;
    WriteLn ('This will delete ',oldh-newh,' lines from each char.');
    WriteLn ('Please enter ',oldh-newh,' numbers from 1 to ',oldh,',');
    WriteLn ('corresponding with the lines to be deleted...');
    WriteLn;
    FOR dummy := 1 TO oldh-newh DO
    BEGIN
        REPEAT
            Write ('Line ',dummy,': ');
            Read (del[dummy]);
            count := 0;
            REPEAT
                INC(count);
            UNTIL (del[dummy] = del[count]) OR (count = dummy)
        UNTIL (del[dummy] > 0) AND (del[dummy] <= oldh) AND (count = dummy);
    END;
    WriteLn ('Deleting ',oldh-newh,' lines from each char...');
    count := 1;
    REPEAT
        BlockRead(input,buffer,1,red);
        dummy := 0;
        REPEAT
            INC (dummy);
        UNTIL (del[dummy] = count) OR (dummy > oldh-newh);
        IF (dummy > oldh-newh) AND (red <> 0) THEN
            BlockWrite (output,buffer,red,written);
        INC(count);
        IF count > oldh THEN
            count := 1;
    UNTIL red = 0;
END; {Shrink}

(* M A I N *)

BEGIN
    WriteLn;
    WriteLn ('FONTASTIC Font-Converter. HeatWave 1992.');
    WriteLn ('----------------------------------------');
    WriteLn;
    IF ParamCount <> 2 THEN
    BEGIN
        WriteLn ('USAGE: FNT2FNT <fontname> <new fontname>');
        WriteLn ('       Fontnames without extension');
        EXIT;
    END;
    Assign (input,Concat(ParamStr(1),'.FNT'));
    Reset (input,1);
    IF IOResult <> 0 THEN
    BEGIN
        WriteLn ('Font not found : "',ParamStr(1),'"...');
        EXIT;
    END;
    Assign (output,Concat(ParamStr(2),'.FNT'));
    ReWrite (output,1);
    BlockRead (input,oldh,1,red);
    WriteLn ('Height of "',ParamStr(1),'": ',oldh);
    Write ('New height of "',ParamStr(2),'" (16=VGA, 14=EGA): ');
    Read(newh);
    IF oldh = newh THEN
    BEGIN
        WriteLn;
        WriteLn ('No changes made...');
        EXIT;
    END;
    BlockWrite (output,newh,1,written);
    IF oldh < newh THEN
        Expand
    ELSE
        Shrink;
    WriteLn;
    WriteLn ('Font succesfully converted...');
    Close(input);
    Close(output);
END. {Fnt2Fnt}
