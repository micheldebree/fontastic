UNIT TextScreen;

(*
    (c) 1992 HeatWave Digital Designs, Leiden/Holland.
    Programmed by Michel "Mad B" de Bree.
*)

INTERFACE

USES Crt;

TYPE ScreenCel = RECORD
                     ch : CHAR;
                     at : BYTE;
                 END;

     ScreenType = Array [0..24,0..79] OF ScreenCel;

     TWindow = RECORD
                    active : BOOLEAN;
                    title : STRING;
                    x,y,
                    xl,yl,
                    at : BYTE;
                    inn : ^ScreenCel;
                    under : ^ScreenCel;
               END;

VAR screen : ^ScreenType;

PROCEDURE WriteAt (x,y : BYTE; s : STRING);
(* Writes s directy to the screen at x,y *)

PROCEDURE Block (x,y,x1,y1,attr : BYTE);
(* Draws a block with attribute attr *)

PROCEDURE Frame (x,y,x1,y1 : BYTE);
(* Draws a frame *)

PROCEDURE DefTWindow (VAR w : TWindow; title : STRING; xl,yl,at : BYTE);
(* Defines a new window *)

PROCEDURE DestroyTWindow (VAR w : TWindow);
(* Destroys a window *)

PROCEDURE FreshTWindow (VAR w : TWindow);
(* Freshens a window's contents *)

PROCEDURE OpenTWindow (VAR w : TWindow; x,y : BYTE);
(* Puts a window on the screen *)

PROCEDURE CloseTWindow (VAR w : TWindow);
(* Takes a window of the screen *)

PROCEDURE ClearTWindow (VAR w : TWindow);
(* Clears window's contents *)

PROCEDURE WWrite (VAR w : TWindow; x,y : BYTE; s : STRING);
(* Writes s to a window at x,y *)

PROCEDURE SelectWLine (VAR w : TWindow; VAR s : BYTE; VAR t : STRING);
(* Waits until a horizontal line from a window is selected. The current line
   is displayed by an inverted bar which is moved using the cursor-keys and
   selecting by SPACE or ENTER. It returns the line number and the contents
   of the line *)

IMPLEMENTATION

PROCEDURE DefTWindow (VAR w : TWindow; title : STRING; xl,yl,at : BYTE);

VAR size : WORD;
    hulp : ^ScreenCel;
    xc,yc : BYTE;

BEGIN
    w.xl := xl;
    w.yl := yl;
    w.at := at;
    w.active := FALSE;
    w.title := title;
    size := w.xl * w.yl * 3;
    GetMem (w.inn,size);
    hulp := ADDR(w.inn^);
    FOR yc := 0 TO w.yl-2 DO
    BEGIN
        FOR xc := 0 TO w.xl-2 DO
        BEGIN
            hulp^.ch := ' ';
            hulp^.at := at;
            INC(hulp);
        END;
    END;
END; {DefTWindow}

PROCEDURE DestroyTWindow (VAR w : TWindow);
BEGIN
    Dispose (w.inn);
END; {DestroyTWindow}

PROCEDURE FreshTWindow (VAR w : TWindow);

VAR xc,yc : BYTE;
    hulp : ^ScreenCel;

BEGIN
    IF w.active THEN
    BEGIN
        hulp := ADDR(w.inn^);
        FOR yc := w.y+1 TO w.y+w.yl-1 DO
        BEGIN
            FOR xc := w.x+1 TO w.x+w.xl-1 DO
            BEGIN
                screen^[yc,xc].ch := hulp^.ch;
                screen^[yc,xc].at := hulp^.at;
                INC(hulp);
            END;
        END;
    END;
END; {FreshTWindow}

PROCEDURE OpenTWindow (VAR w : TWindow; x,y : BYTE);

VAR size : WORD;
    xc,yc,x1,y1,xr,yr : INTEGER;
    hulp : ^ScreenCel;

BEGIN
    w.active := TRUE;
    w.x := x;
    w.y := y;
    size := w.xl * w.yl * 3;
    GetMem (w.under,size+1);
    hulp := ADDR(w.under^);
    IF (w.xl DIV w.yl) < 1 THEN
    BEGIN
        xr := 1;
        yr := w.yl DIV w.xl;
    END
    ELSE
    BEGIN
        yr := 1;
        xr := w.xl DIV w.yl;
    END;
    FOR yc := w.y TO w.y+w.yl DO
    BEGIN
        FOR xc := w.x TO w.x+w.xl DO
        BEGIN
            hulp^.ch := screen^[yc,xc].ch;
            hulp^.at := screen^[yc,xc].at;
            INC(hulp);
        END;
    END;
    xc := w.x+w.xl DIV 2;
    yc := w.y+w.yl DIV 2;
    x1 := xc;
    y1 := yc;
    WHILE (xc > w.x) OR (yc > w.y) DO
    BEGIN
        Block (xc,yc,x1,y1,w.at);
        Delay (20);
        IF xc > w.x THEN
        BEGIN
            DEC (xc,xr);
            INC (x1,xr);
        END;
        IF (yc > w.y) THEN
        BEGIN
            DEC (yc,yr);
            INC (y1,yr);
        END;
    END;
    xc := 1 + w.x + (w.xl - ORD(w.title[0])) DIV 2;
    Block (w.x,w.y,w.x+w.xl,w.y+w.yl,w.at);
    WriteAt (xc,w.y,w.title);
    FreshTWindow(w);
END; {OpenTWindow}

PROCEDURE CloseTWindow (VAR w : TWindow);

VAR size : WORD;
    x1,y1,x2,y2,xc,yc,xr,yr : BYTE;
    hulp : ^ScreenCel;

BEGIN
    w.active := FALSE;
    IF (w.xl DIV w.yl) < 1 THEN
    BEGIN
        xr := 1;
        yr := w.yl DIV w.xl;
    END
    ELSE
    BEGIN
        yr := 1;
        xr := w.xl DIV w.yl;
    END;
    x1 := w.x;
    y1 := w.y;
    x2 := w.x+w.xl;
    y2 := w.y+w.yl;
    WHILE (x2-x1 > 1) OR (y2-y1 > 1) DO
    BEGIN
        Delay(15);
        IF x2-x1 > 1 THEN
        BEGIN
            INC(x1,xr);
            DEC(x2,xr);
        END;
        IF y2-y1 > 1 THEN
        BEGIN
            INC(y1,yr);
            DEC(y2,yr);
        END;
        hulp := ADDR(w.under^);
        FOR yc := w.y TO w.y+w.yl DO
        BEGIN
            FOR xc := w.x TO w.x+w.xl DO
            BEGIN
                 IF (xc < x1) OR (xc > x2) OR (yc < y1) OR (yc > y2) THEN
                 BEGIN
                     screen^[yc,xc].ch := hulp^.ch;
                     screen^[yc,xc].at := hulp^.at;
                 END;
                 INC(hulp);
            END;
        END;
        Frame(x1,y1,x2,y2);
    END;
    hulp := ADDR(w.under^);
    FOR yc := w.y TO w.y+w.yl DO
    BEGIN
        FOR xc := w.x TO w.x+w.xl DO
        BEGIN
            screen^[yc,xc].ch := hulp^.ch;
            screen^[yc,xc].at := hulp^.at;
            INC (hulp);
        END;
    END;
    Dispose (w.under);
END; {CloseTWindow}

PROCEDURE ClearTWindow (VAR w : TWindow);

VAR x,y : BYTE;
    pos : ^ScreenCel;

BEGIN
    pos := ADDR(w.inn^);
    FOR y := 1 TO w.yl DO
    BEGIN
        FOR x := 1 to w.xl DO
        BEGIN
            pos^.ch := ' ';
            pos^.at := w.at;
            INC(pos);
        END;
    END;
END; {ClearTWindow}

PROCEDURE WWrite (VAR w : TWindow; x,y : BYTE; s : STRING);

VAR c,i : WORD;
    hulp : ^ScreenCel;

BEGIN
    i := 1;
    hulp := ADDR(w.inn^);
    c := y * (w.xl-1) + x;
    INC(hulp,c);
    WHILE i <= ORD(s[0]) DO
    BEGIN
        hulp^.ch := s[i];
        INC(hulp);
        INC(i);
    END;
END; {WWrite}

PROCEDURE SelectWLine (VAR w : TWindow; VAR s : BYTE; VAR t : STRING);

VAR position,hulp : ^ScreenCel;
    key : CHAR;
    choisemade : BOOLEAN;
    i : BYTE;

    PROCEDURE Highlight;

    VAR i,a : BYTE;

    BEGIN
        hulp := ADDR(position^);
        FOR i := 1 TO w.xl-1 DO
        BEGIN
            hulp^.at := (16*(hulp^.at MOD 16)) + (hulp^.at DIV 16);
            INC(hulp);
        END;
    END; {Highlight}

BEGIN
    choisemade := FALSE;
    s := 0;
    REPEAT
        position := ADDR(w.inn^);
        INC (position,s*(w.xl-1));
        Highlight;
        FreshTWindow (w);
        key := ReadKey;
        Highlight;
        IF (key = ' ') OR (key = CHR(13)) THEN
            choisemade := TRUE;
        IF key = CHR(27) THEN
        BEGIN
            choisemade := TRUE;
            t[0] := CHR(0);
            s := 255;
        END;
        IF key = CHR(72) THEN
        BEGIN
            IF s = 0 THEN
                s := w.yl-2
            ELSE
                DEC(s);
        END;
        IF key = CHR(80) THEN
        BEGIN
            IF s = w.yl-2 THEN
                s := 0
            ELSE
                INC(s);
        END;
    UNTIL choisemade;
    IF s <> 255 THEN
    BEGIN
        INC(position,w.xl-2);
        i := w.xl-1;
        WHILE (position^.ch = ' ') AND (i > 0) DO
        BEGIN
            DEC(position);
            DEC(i);
        END;
        t[0] := CHR(i);
        WHILE i > 0 DO
        BEGIN
            t[i] := position^.ch;
            DEC(position);
            DEC(i);
        END;
    END;
    FreshTWindow(w);
END; {SelectWline}

PROCEDURE WriteAt (x,y : BYTE;s : STRING);

VAR i : INTEGER;

BEGIN
    i := 1;
    WHILE i <= ORD(s[0]) DO
    BEGIN
        screen^[y,x+i-1].ch := s[i];
        INC(i);
    END;
END; {WriteAt}

PROCEDURE Block (x,y,x1,y1,attr : BYTE);

VAR xc,yc : BYTE;

BEGIN
    FOR yc := y TO y1 DO
    BEGIN
        FOR xc := x TO x1 DO
        BEGIN
            screen^[yc,xc].at := attr;
        END;
    END;
    FOR yc := y+1 TO y1-1 DO
    BEGIN
        FOR xc := x+1 TO x1-1 DO
        BEGIN
            screen^[yc,xc].ch := ' ';
        END;
    END;
    screen^[y,x].ch := CHR(201);
    screen^[y1,x1].ch := CHR(188);
    screen^[y,x1].ch := CHR(187);
    screen^[y1,x].ch := CHR(200);
    FOR yc := y+1 TO y1-1 DO
    BEGIN
        screen^[yc,x].ch := CHR(186);
        screen^[yc,x1].ch := CHR(186);
    END;
    FOR xc := x+1 TO x1-1 DO
    BEGIN
        screen^[y,xc].ch := CHR(205);
        screen^[y1,xc].ch := CHR(205);
    END;
END; {Block}

PROCEDURE Frame (x,y,x1,y1 : BYTE);

VAR xc,yc : BYTE;

BEGIN
    screen^[y,x].ch := CHR(201);
    screen^[y1,x1].ch := CHR(188);
    screen^[y,x1].ch := CHR(187);
    screen^[y1,x].ch := CHR(200);
    FOR yc := y+1 TO y1-1 DO
    BEGIN
        screen^[yc,x].ch := CHR(186);
        screen^[yc,x1].ch := CHR(186);
    END;
    FOR xc := x+1 TO x1-1 DO
    BEGIN
        screen^[y,xc].ch := CHR(205);
        screen^[y1,xc].ch := CHR(205);
    END;
END; {Frame}

(* INITIALISATION *)

BEGIN
   IF MEM[$0000:$0449] = 7 THEN
      screen := PTR($B000,$0000)
   ELSE
      screen := PTR($B800,$0000);
END. {TextScreen}