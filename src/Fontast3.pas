PROGRAM Fontastic;
{$I-}

(* Copyright 1992 by HeatWave Digital Designs.
   Program by M. de Bree *)

USES Crt,Dos,TextScreen;

CONST power : ARRAY [0..7] OF BYTE = (128,64,32,16,8,4,2,1);

VAR mode        : (vga,ega);
    QuitFontastic,
    endedit,
    fontchanged : BOOLEAN;
    key         : CHAR;
    reg         : REGISTERS;
    curchar,
    height,
    choise,
    cursorx,
    cursory,
    oldx,
    oldy        : BYTE;
    font        : ARRAY [0..16*256] OF BYTE;
    buffer      : ARRAY [0..15] OF BYTE;
    name,
    dumname     : STRING;
    mainmenu,
    filemenu,
    filename,
    editmenu,
    helpwindow,
    editfield,
    charfield,
    message,
    directory,
    background,
    select      : TWindow;

(* ---------
   UTILITIES
   --------- *)

PROCEDURE ClearKey;
(* Flushes the keyboard-buffer into nowhere *)

VAR k : CHAR;

BEGIN
    WHILE KeyPressed DO
    BEGIN
        k := ReadKey;
    END;
END; {ClearKey}

PROCEDURE HideCursor;
(* Hides the text-cursor *)

BEGIN
    reg.AH := 1;
    reg.CH := $20;
    reg.CL := $07;
    INTR ($10,reg);
END; {HideCursor}

PROCEDURE ShowCursor;
(* Shows the text-cursor *)

BEGIN
    reg.AH := 1;
    IF mode = vga THEN
    BEGIN
        reg.CH := 10;
        reg.CL := 11;
    END
    ELSE
    BEGIN
        reg.CH := 7;
        reg.CL := 9;
    END;
    INTR ($10,reg);
END; {ShowCursor}

(* -------------------
   TERMINATE FONTASTIC
   ------------------- *)

PROCEDURE Leave;
(* Leave FONTASTIC *)

BEGIN
    CloseTWindow (background);
    IF oldy > 25 THEN
        oldy := 25;
    GotoXY(oldx,oldy);
    TextColor(7);
    TextBackground(0);
    ShowCursor;
    QuitFontastic := TRUE;
    reg.AH := $11;
    reg.AL := $03;
    reg.BL := $00;
    INTR($10,reg);
END; {Leave}

PROCEDURE Quit;
(* Quit FONTASTIC *)

VAR k : CHAR;

BEGIN
    IF fontchanged THEN
    BEGIN
         ClearTWindow(message);
         WWrite (message,0,0,'        Font NOT saved! Really quit? (Y/N)');
         OpenTWindow (message,13,10);
         ClearKey;
         k := ReadKey;
         CloseTWindow (message);
         IF (k = 'y') OR (k = 'Y') THEN
              Leave
         ELSE
         BEGIN
             OpenTWindow (charfield,23,7);
             OpenTWindow (mainmenu,16,7);
         END;
    END
    ELSE
        Leave;
END; {Quit}

(* -------------------
   CHARACTER GENERATOR
   ------------------- *)

PROCEDURE InstallFont (start,count : WORD);
(* Installs the user font *)

BEGIN
    reg.AH := $11;                         (* Command: character-generator *)
    reg.AL := $10;                               (* Sub-command: load font *)
    reg.ES := SEG(font[start*height]);              (* ES:BP -> user table *)
    reg.BP := OFS(font[start*height]);
    reg.DX := start;                                    (* Start with char *)
    reg.CX := count;                         (* Count of patterns to store *)
    reg.BH := height;                                 (* Bytes per pattern *)
    reg.BL := 1;                                           (* second block *)
    INTR ($10,reg);                                  (* Call interrupt $10 *)
    reg.AL := 3;                                    (* Set Block specifier *)
    reg.BL := 4;
    INTR ($10,reg);
END; {InstallFont}

PROCEDURE GetRomFont;
(* Copies the ROM font to the user font *)

VAR i : WORD;

BEGIN
    IF mode = ega THEN
    BEGIN
        reg.BH := $02;
        reg.BL := 1;
        height := 14;
    END
    ELSE
    BEGIN
        reg.BH := $06;
        reg.BL := 1;
        height := 16;
    END;
    reg.AX := $1130;
    INTR ($10,reg);
    FOR i := 0 TO 256*height DO
    BEGIN
        font[i] := MEM[reg.ES:reg.BP+i];
    END;
END; {GetRomFont}

(* --------------
   INITIALISATION
   -------------- *)

PROCEDURE WallPaper; EXTERNAL;
{$L WALLPAP.OBJ}

PROCEDURE InstallWallPaper;
(* Glues the wallpaper to the wall *)

VAR paper : ^CHAR;
    x,y   : BYTE;

BEGIN
    paper := ADDR(WallPaper);
    x := 0;
    y := 0;
    WHILE paper^ <> '*' DO
    BEGIN
        IF paper^ = CHR($0D) THEN
        BEGIN
            INC(paper);
            INC(paper);
            x := 0;
            INC(y);
        END
        ELSE
        BEGIN
            WWrite(background,x,y,paper^);
            INC(paper);
            INC (x);
        END;
    END;
END; {InstallWallPaper}

PROCEDURE Init;
(* Inits some things concerning windows etc. *)

VAR xc,yc : BYTE;
    hulp  : ^ScreenCel;

BEGIN
    oldx := WhereX;
    oldy := WhereY;
    cursorx := 0;
    cursory := 0;
    HideCursor;
    FontChanged := FALSE;
    QuitFontastic := FALSE;
    DefTWindow (charfield,'Char-Map',33,9,$07);
    DefTWindow (mainmenu,'Menu',5,4,$70);
    DefTWindow (filemenu,'Files',10,5,$70);
    DefTWindow (helpwindow,'Help - Cursor up/down to move - ESC to Exit',79,11,$20);
    DefTWindow (editmenu,'Edit',10,8,$70);
    DefTWindow (select,'Choose VideoMode',32,4,$70);
    DefTWindow (filename,'Name?',9,2,$70);
    DefTWindow (message,'Message:',52,2,$47);
    DefTWindow (directory,'Disk',9,22,$70);
    DefTWindow (background,'',79,24,$17);
    WWrite (mainmenu,0,0,'QuitFileHelp');
    WWrite (filemenu,0,0,'Exit     Load FontSave FontDos Shell');
    WWrite (editmenu,0,0,'Quit EditScroll   Flip     Clear    Invert');
    WWrite (editmenu,0,5,'Read     Write');
    WWrite (select,0,0,'Video Graphics Array      (VGA)');
    WWrite (select,0,1,'Enhanced Graphics Adaptor (EGA)');
    WWrite (select,0,2,'Quit FONTASTIC now        (DUM)');
    hulp := ADDR(charfield.inn^);
    FOR yc:= 0 TO 7 DO
    BEGIN
        FOR xc := 0 TO 31 DO
        BEGIN
             WWrite (charfield,xc,yc,CHR(yc*32+xc));
             hulp^.at := $0F;
             INC(hulp);
        END;
    END;
        FOR yc := 0 TO 15 DO
        BEGIN
            buffer[yc] := 0;
        END;
    InstallWallPaper;
    cursorx := 0;
    cursory := 0;
    ClearKey;
END; {Init}

(* ----
   HELP
   ---- *)

PROCEDURE HelpText; EXTERNAL;
{$L HELPTEXT.OBJ}

PROCEDURE Help;
(* Help! *)

VAR start, first, pos : ^CHAR;
    x,y : BYTE;
    k : CHAR;

BEGIN
    ClearTWindow (helpwindow);
    OpenTWindow(helpwindow,0,6);
    start := @HelpText;
    x := 0;
    first := start;
    WHILE k <> CHR(27) DO
    BEGIN
    pos := first;
    y := 0;
    x := 0;
    ClearTWindow(helpwindow);
    WHILE (y < helpwindow.yl-1) AND (pos^ <> '*') DO
    BEGIN
        IF pos^ = CHR($0D) THEN
        BEGIN
            INC(y);
            x := 0;
            INC(pos);
        END
        ELSE
        BEGIN
            WWrite(helpwindow,x,y,pos^);
            INC(x);
        END;
        INC(pos);
    END;
    FreshTWindow(helpwindow);
    k := ReadKey;
    IF (k = CHR(80)) AND (y = helpwindow.yl-1) THEN
    BEGIN
        WHILE first^ <> CHR($0A)  DO
        BEGIN
            INC(first);
        END;
        INC(first);
    END;
    IF (k = CHR(72)) AND (first <> start) THEN
    BEGIN
        DEC(first);
        DEC(first);
        WHILE (first^ <> CHR($0A)) AND (first <> start) DO
        BEGIN
            DEC(first);
        END;
        IF first <> start THEN
            INC(first);
    END;
    END;
    CloseTWindow(helpwindow);
END; {Help}

(* --------------
   CHAR-UTILITIES
   -------------- *)

PROCEDURE WriteZoom (c : BYTE);
(* Writes a zoomed-out version of character c to the window 'editfield' *)

VAR xc,yc : BYTE;
    a : WORD;

BEGIN
    a := c * height;
    FOR yc := 0 TO height-1 DO
    BEGIN
        FOR xc := 0 TO 7 DO
        BEGIN
            IF (font[a+yc] AND power[xc]) <> 0 THEN
                WWrite (editfield,xc*2,yc,'лл')
            ELSE
                WWrite (editfield,xc*2,yc,'..');
        END;
    END;
END; {WriteZoom}

PROCEDURE FlipPixel (x,y : BYTE);
(* Flips a pixel at x,y in the current char *)

VAR a : WORD;

BEGIN
    fontchanged := TRUE;
    a := height * curchar + y;
    IF (font[a] AND power[x] = 0) THEN
        font[a] := font[a] OR power[x]
    ELSE
        font[a] := font[a] AND (255-power[x]);
    WriteZoom(curchar);
    FreshTWindow(editfield);
END; {FlipPixel}

(* ---------
   SCROLLING
   --------- *)

PROCEDURE ScrollUp;
(* Scrolls up the current char one line *)

VAR buff : BYTE;
    i,a : WORD;

BEGIN
    a := curchar * height;
    buff := font[a];
    FOR i := a TO a + height -2 DO
    BEGIN
        font[i] := font[i+1];
    END;
    font[i+1] := buff;
    WriteZoom (curchar);
    FreshTWindow (editfield);
END; {ScrollUp}

PROCEDURE ScrollDown;
(* Scrolls down the current char one line *)

VAR buff : BYTE;
    i,a : WORD;

BEGIN
    a := curchar * height;
    buff := font[a+height-1];
    FOR i := a + height -2 DOWNTO a DO
    BEGIN
        font[i+1] := font[i];
    END;
    font[a] := buff;
END; {ScrollDown}

PROCEDURE ScrollRight;
(* Scrolls right the current char one pixel *)

VAR i,a : WORD;
    buff : BYTE;

BEGIN
    a := curchar * height;
    FOR i := a TO a + height-1 DO
    BEGIN
        buff := font[i] AND 1;
        font[i] := font[i] DIV 2;
        font[i] := font[i] OR (buff * 128);
    END;
END; {ScrollRight}

PROCEDURE ScrollLeft;
(* Scrolls left the current char one pixel *)

VAR i,a : WORD;
    buff : BYTE;
BEGIN
    a := curchar * height;
    FOR i := a TO a + height-1 DO
    BEGIN
        buff := font[i] AND 128;
        font[i] := font[i] * 2;
        font[i] := font[i] OR (buff DIV 128);
    END;
END; {ScrollLeft}

PROCEDURE Scroll;
(* Controls the scrolling by mouse & keyboard *)

VAR key : CHAR;

BEGIN
    WHILE key <> CHR(27) DO
    BEGIN
        IF KeyPressed THEN
        BEGIN
            key := ReadKey;
            IF key = CHR(72) THEN
                ScrollUp;
            IF key = CHR(80) THEN
                ScrollDown;
            IF key = CHR(75) THEN
                ScrollLeft;
            IF key = CHR(77) THEN
                ScrollRight;
            WriteZoom (curchar);
            FreshTWindow (editfield);
        END;
    END;
END; {Scroll}

(* --------
   FLIPPING
   -------- *)

PROCEDURE HFLip;
(* Flips current char horizontal *)

VAR buffer : ARRAY [0..15] OF BYTE;
    a,i,ii : WORD;
    bit : BYTE;

BEGIN
    a := curchar * height;
    FOR i := 0 TO height-1 DO
    BEGIN
        buffer[i] := font[a+i];
    END;
    FOR i := 0 TO height-1 DO
    BEGIN
        FOR ii := 0 TO 7 DO
        BEGIN
            bit := buffer[i] AND 128;
            buffer[i] := buffer[i] SHL 1;
            font[a+i] := font[a+i] SHR 1;
            font[a+i] := font[a+i] OR bit;
        END;
    END;
END; {HFlip}

PROCEDURE VFLip;
(* Flips current char vertical *)

VAR buffer : ARRAY [0..15] OF BYTE;
    a,i : WORD;

BEGIN
    a := curchar * height;
    FOR i := 0 TO height-1 DO
    BEGIN
        buffer[i] := font[a+i];
    END;
    FOR i := 0 TO height-1 DO
    BEGIN
        font[a+i] := buffer[height-i-1];
    END;
END; {VFlip}

PROCEDURE Flip;

VAR key : CHAR;

BEGIN
    ClearKey;
    key := CHR(0);
    WHILE key <> CHR(27) DO
    BEGIN
        IF KeyPressed THEN
        BEGIN
            key := ReadKey;
            IF (key = CHR(72)) OR (key = CHR(80)) THEN
                VFlip;
            IF (key = CHR(75)) OR (key = CHR(77)) THEN
                HFlip;
            WriteZoom (curchar);
            FreshTWindow (editfield);
        END;
    END;
END; {Flip}

(* --------
   FONT I/O
   -------- *)

PROCEDURE SaveFont;
(* Saves the font to disk *)

VAR output : FILE;
    i,result : WORD;

BEGIN
    IF name[0] <> CHR(4) THEN
    BEGIN
        Assign (output,name);
        Reset (output,1);
        key := 'y';
        IF IOResult = 0 THEN
        BEGIN
            ClearTWindow (message);
            WWrite (message,0,0,'       Font already exists! Overwrite? (Y/N)');
            OpenTWindow (message,14,3);
            ClearKey;
            key := Readkey;
            CloseTWindow(message);
        END;
        IF (key = 'y') OR (key = 'Y') THEN
        BEGIN
            ReWrite (output,1);
            BlockWrite (output,height,1,result);
            BlockWrite (output,font,height*256,result);
            FontChanged := FALSE;
         END;
         Close(output);
    END;
END; {SaveFont}

PROCEDURE LoadFont;
(* Loads a font from disk *)

VAR from : FILE ;
    result,i : WORD;
    heightc : BYTE;


BEGIN
    IF name[0] <> CHR(4) THEN
    BEGIN
        Assign (from,name);
        Reset (from,1);
        IF IOResult <> 0 THEN
        BEGIN
             ClearTWindow (message);
             WWrite(message,12,0,'File not found:');
             WWrite(message,28,0,name);
             OpenTWindow(message,14,18);
             REPEAT
             UNTIL KeyPressed;
             CloseTWindow(message);
             ClearKey;
        END
        ELSE
        BEGIN
            BlockRead (from,heightc,1,result);
            IF ((mode = VGA) AND (heightc = 16)) OR
               ((mode = EGA) AND (heightc = 14)) THEN
            BEGIN
                BlockRead(from,font,heightc*256,result);
                InstallFont (0,256);
                fontchanged := FALSE;
                height := heightc;
            END;
            Close(from);
        END;
    END;
END; {LoadFont}

PROCEDURE EnterName (y : BYTE);
(* Lets the user enter the name on the keyboard *)

VAR x : BYTE;
    key : CHAR;

BEGIN
    x := 1;
    OpenTWindow (filename,directory.x,y);
    GotoXY (filename.x+2,filename.y+2);
    TextColor (filename.at AND 15);
    TextBackground (filename.at SHR 4);
    ShowCursor;
    REPEAT
        key := UpCase(ReadKey);
        IF key = CHR(0) THEN
        BEGIN
            key := ReadKey;
            key := CHR(0);
        END;
        IF (key >= '0') AND (key <= 'Z') AND (x < 9) THEN
        BEGIN
            Write(key);
            name[x] := key;
            INC(x);
        END;
        IF (key = CHR(8)) AND (x > 1) THEN
        BEGIN
            Write(key,' ',key);
            DEC(x);
        END;
    UNTIL (key = CHR(13)) OR (key = CHR(27));
    name[0] := CHR(x-1);
    IF key = CHR(27) THEN
        name[0] := CHR(0);
    HideCursor;
    CloseTWindow (filename);
END; {EnterName}

PROCEDURE PickName;
(* Picks a name from the current directory *)

VAR s : SearchRec;
    x,y,choise : BYTE;
    hulp : ^ScreenCel;

BEGIN
    name[0] := CHR(0);
    OpenTWindow (directory,58,1);
    FindFirst('*.FNT',$3F,s);
    REPEAT
        y := 1;
        ClearTWindow (directory);
        WWrite(directory,0,0,' <Name>');
        WHILE (DosError = 0) AND (y < 20) DO
        BEGIN
            x := 1;
            WHILE s.name[x] <> '.' DO
            BEGIN
                WWrite (directory,x-1,y,s.name[x]);
                INC(x);
            END;
            INC(y);
            FindNext (s);
        END;
        IF (y = 20) AND (DosError = 0) THEN
            WWrite (directory,0,20,' <More>');
        FreshTWindow (directory);
        SelectWline (directory,choise,name);
    UNTIL (choise <> 20) OR (DosError <> 0);
    IF choise <> 255 THEN
    BEGIN
        IF (name[0] = CHR(0)) OR (name[1] = ' ') THEN
            EnterName (choise+1);
    END;
    name := Concat(name,'.FNT');
    CloseTWindow (directory);
END; {PickName}

PROCEDURE DosShell;
BEGIN
    CloseTWindow(charfield);
    CloseTWindow(background);
    GotoXY(oldx,oldy);
    TextColor(7);
    TextBackground(0);
    WriteLn;
    Write('Type "EXIT" to return to FONTASTIC...');
    ShowCursor;
    Exec (GetEnv('COMSPEC'),'');
    HideCursor;
    oldx := WhereX;
    oldy := WhereY;
    OpenTWindow(background,0,0);
    OpenTWindow(charfield,23,7);
END; {DosShell}

PROCEDURE Files;
(* The menu for the files *)

VAR k : CHAR;

BEGIN
    HideCursor;
    OpenTWindow (filemenu,23,1);
    SelectWline (filemenu,choise,dumname);
    CloseTWindow (filemenu);
    IF choise = 1 THEN
    BEGIN
        IF fontchanged THEN
        BEGIN
            WWrite (message,0,0,'Current font NOT saved! Really load new font? (Y/N)');
            OpenTWindow (message,14,3);
            ClearKey;
            k := ReadKey;
            CloseTWindow(message);
            IF (k = 'y') OR (k = 'Y') THEN
            BEGIN
                PickName;
                LoadFont;
            END;
        END
        ELSE
        BEGIN
            PickName;
            LoadFont;
        END;
    END;
    IF choise = 2 THEN
    BEGIN
        PickName;
        SaveFont;
    END;
    IF choise = 3 THEN
        DosShell;
END; {Files}

(* ---------------------
   OTHER EDIT-SELECTIONS
   --------------------- *)

PROCEDURE ClearChar;
(* Clears the current char *)

VAR i : WORD;

BEGIN
    FOR i := height*curchar TO height*curchar + height-1 DO
    BEGIN
        font[i] := 0;
    END;
    WriteZoom (curchar);
    FreshTWindow (editfield);
END; {ClearChar}

PROCEDURE Invert;
(* Inverts the current char *)

VAR i : WORD;

BEGIN
    FOR i := height*curchar TO height*curchar + height-1 DO
    BEGIN
        font[i] := font[i] XOR 255;
    END;
    WriteZoom (curchar);
    FreshTWindow (editfield);
END; {Invert}

PROCEDURE WriteBuffer;
(* Writes the buffer to the current char *)

VAR i : BYTE;
    a : WORD;

BEGIN
    a := curchar * height;
    FOR i := 0 TO height-1 DO
    BEGIN
        buffer[i] := font[a+i];
    END;
    WriteZoom (curchar);
    FreshTWindow (editfield);
END; {WriteBuffer}

PROCEDURE ReadBuffer;
(* Reads current char into the buffer *)

VAR i : BYTE;
    a : WORD;

BEGIN
    a := curchar * height;
    FOR i := 0 TO height-1 DO
    BEGIN
        font[a+i] := buffer[i];
    END;
    WriteZoom (curchar);
    FreshTWindow (editfield);
END; {ReadBuffer}

PROCEDURE EditChoise;
(* The edit-menu *)

VAR choise : BYTE;

BEGIN
    HideCursor;
    SelectWLine (editmenu,choise,dumname);
    CASE choise OF
       1 : Scroll;
       2 : Flip;
       3 : ClearChar;
       4 : Invert;
       5 : ReadBuffer;
       6 : WriteBuffer;
    END;
    IF choise <> 0 THEN
        FontChanged := TRUE
    ELSE
        endedit := TRUE;
    ShowCursor;
END; {EditChoise}

(* ---------
   MAIN MENU
   --------- *)

PROCEDURE Main;
(* The main menu *)

VAR choise : BYTE;

BEGIN
    HideCursor;
    SelectWLine (mainmenu,choise,dumname);
    IF choise <> 255 THEN
    BEGIN
        IF choise = 0 THEN
        BEGIN
            CloseTWindow(mainmenu);
            CloseTWindow(charfield);
            Quit;
        END;
        IF choise = 1 THEN
        BEGIN
            CloseTWindow(mainmenu);
            Files;
            OpenTWindow (mainmenu,16,7);
        END;
        IF choise = 2 THEN
        BEGIN
           CloseTWindow(mainmenu);
           Help;
           OpenTWindow (mainmenu,16,7);
        END;
    END;
    ShowCursor;
END; {Main}

(* ----------------
   EDITING THE CHAR
   ---------------- *)

PROCEDURE EditChar (x,y : BYTE);
(* Controls the editing of the current char *)

VAR i : WORD;
    key : CHAR;
    p : ^ScreenCel;
    newx,newy,mousex,mousey : BYTE;

BEGIN
    curchar := y * 32 + x;
    endedit := FALSE;
    WriteZoom (curchar);
    CloseTWindow (mainmenu);
    x := charfield.x+x-7;
    y := charfield.y+y-7;
    cursorx := 0;
    cursory := 0;
    Str (curchar,editfield.title);
    OpenTWindow (editfield,x,y);
    OpenTWindow (editmenu,x+19,y);
    GotoXY (editfield.x+cursorx+2,editfield.y+cursory+2);
    ShowCursor;
    newx := editfield.x+1;
    newy := editfield.y+1;
    ClearKey;
    REPEAT
        screen^[newy,newx].at := $03;
        screen^[newy,newx+1].at := $03;
        IF KeyPressed THEN
        BEGIN
            newx := editfield.x+cursorx+1;
            newy := editfield.y+cursory+1;
            screen^[newy,newx].at := editfield.at;
            screen^[newy,newx+1].at := editfield.at;
            key := ReadKey;
            IF (key = CHR(72)) AND (cursory > 0) THEN
                DEC(cursory);
            IF (key = CHR(80)) AND (cursory < editfield.yl-2) THEN
                INC(cursory);
            IF (key = CHR(75)) AND (cursorx > 0) THEN
                DEC(cursorx,2);
            IF (key = CHR(77)) AND (cursorx < editfield.xl-4) THEN
                INC(cursorx,2);
            IF (key = ' ') OR (key = CHR(13)) THEN
            BEGIN
                FlipPixel(cursorx DIV 2,cursory);
            END;
            IF key = CHR(27) THEN
                EditChoise;
            newx := editfield.x+cursorx+1;
            newy := editfield.y+cursory+1;
            GotoXY (newx+1,newy+1);
        END;
    UNTIL endedit;
    HideCursor;
    InstallFont (curchar,1);
    HideCursor;
    CloseTWindow (editfield);
    CloseTWindow (editmenu);
    OpenTWindow (mainmenu,16,7);
    cursorx := curchar MOD 32;
    cursory := curchar DIV 32;
END; {EditChar}

(* ----------------------
   M A I N  P R O G R A M
   ---------------------- *)

BEGIN
    Init;
    OpenTWindow(background,0,0);
    OpenTWindow (select,23,5);
    REPEAT
        SelectWline (select,choise,dumname);
        IF choise = 0 THEN
            mode := vga;
        IF choise = 1 THEN
            mode := ega;
        IF choise = 2 THEN
        BEGIN
            CloseTWindow(select);
            Quit;
            EXIT;
        END;
    UNTIL choise <> 255;
    CloseTWindow (select);
    reg.BL := $00;
    IF mode = ega THEN
        reg.AX := $1111
    ELSE
        reg.AX := $1114;
    INTR($10,reg);
    GetRomFont;
    DefTWindow (editfield,'Character',17,height+1,$07);
    InstallFont (0,256);
    HideCursor;
    OpenTWindow (charfield,23,7);
    OpenTWindow (mainmenu,16,7);
    ShowCursor;
    WHILE NOT QuitFontastic DO
    BEGIN
        GotoXY (charfield.x+cursorx+2,charfield.y+cursory+2);
        key := ReadKey;
        IF (key = CHR(72)) AND (cursory > 0) THEN
            DEC(cursory);
        IF (key = CHR(80)) AND (cursory < charfield.yl-2) THEN
            INC(cursory);
        IF (key = CHR(75)) AND (cursorx > 0) THEN
            DEC(cursorx);
        IF (key = CHR(77)) AND (cursorx < charfield.xl-2) THEN
            INC(cursorx);
        IF (key = ' ') OR (key = CHR(13)) THEN
        BEGIN
            HideCursor;
            EditChar(cursorx,cursory);
            ShowCursor;
        END;
        IF key = CHR(27) THEN
            Main;
    END;
END. {Fontastic}

