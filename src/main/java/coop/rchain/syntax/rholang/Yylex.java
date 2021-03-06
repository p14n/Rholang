// This JLex file was machine-generated by the BNF converter
package coop.rchain.syntax.rholang;
import java_cup.runtime.*;


public class Yylex implements java_cup.runtime.Scanner {
	private final int YY_BUFFER_SIZE = 512;
	private final int YY_F = -1;
	private final int YY_NO_STATE = -1;
	private final int YY_NOT_ACCEPT = 0;
	private final int YY_START = 1;
	private final int YY_END = 2;
	private final int YY_NO_ANCHOR = 4;
	private final int YY_BOL = 65536;
	private final int YY_EOF = 65537;

  String pstring = new String();
  public int line_num() { return (yyline+1); }
  public String buff() {return new String(yy_buffer,yy_buffer_index,10).trim();}
	private java.io.BufferedReader yy_reader;
	private int yy_buffer_index;
	private int yy_buffer_read;
	private int yy_buffer_start;
	private int yy_buffer_end;
	private char yy_buffer[];
	private int yyline;
	private boolean yy_at_bol;
	private int yy_lexical_state;

	public Yylex (java.io.Reader reader) {
		this ();
		if (null == reader) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(reader);
	}

	public Yylex (java.io.InputStream instream) {
		this ();
		if (null == instream) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(new java.io.InputStreamReader(instream));
	}

	private Yylex () {
		yy_buffer = new char[YY_BUFFER_SIZE];
		yy_buffer_read = 0;
		yy_buffer_index = 0;
		yy_buffer_start = 0;
		yy_buffer_end = 0;
		yyline = 0;
		yy_at_bol = true;
		yy_lexical_state = YYINITIAL;
	}

	private boolean yy_eof_done = false;
	private final int STRING = 5;
	private final int ESCAPED = 6;
	private final int YYINITIAL = 0;
	private final int COMMENT = 1;
	private final int CHAREND = 4;
	private final int CHARESC = 3;
	private final int CHAR = 2;
	private final int yy_state_dtrans[] = {
		0,
		69,
		71,
		73,
		75,
		77,
		79
	};
	private void yybegin (int state) {
		yy_lexical_state = state;
	}
	private int yy_advance ()
		throws java.io.IOException {
		int next_read;
		int i;
		int j;

		if (yy_buffer_index < yy_buffer_read) {
			return yy_buffer[yy_buffer_index++];
		}

		if (0 != yy_buffer_start) {
			i = yy_buffer_start;
			j = 0;
			while (i < yy_buffer_read) {
				yy_buffer[j] = yy_buffer[i];
				++i;
				++j;
			}
			yy_buffer_end = yy_buffer_end - yy_buffer_start;
			yy_buffer_start = 0;
			yy_buffer_read = j;
			yy_buffer_index = j;
			next_read = yy_reader.read(yy_buffer,
					yy_buffer_read,
					yy_buffer.length - yy_buffer_read);
			if (-1 == next_read) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}

		while (yy_buffer_index >= yy_buffer_read) {
			if (yy_buffer_index >= yy_buffer.length) {
				yy_buffer = yy_double(yy_buffer);
			}
			next_read = yy_reader.read(yy_buffer,
					yy_buffer_read,
					yy_buffer.length - yy_buffer_read);
			if (-1 == next_read) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}
		return yy_buffer[yy_buffer_index++];
	}
	private void yy_move_end () {
		if (yy_buffer_end > yy_buffer_start &&
		    '\n' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
		if (yy_buffer_end > yy_buffer_start &&
		    '\r' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
	}
	private boolean yy_last_was_cr=false;
	private void yy_mark_start () {
		int i;
		for (i = yy_buffer_start; i < yy_buffer_index; ++i) {
			if ('\n' == yy_buffer[i] && !yy_last_was_cr) {
				++yyline;
			}
			if ('\r' == yy_buffer[i]) {
				++yyline;
				yy_last_was_cr=true;
			} else yy_last_was_cr=false;
		}
		yy_buffer_start = yy_buffer_index;
	}
	private void yy_mark_end () {
		yy_buffer_end = yy_buffer_index;
	}
	private void yy_to_mark () {
		yy_buffer_index = yy_buffer_end;
		yy_at_bol = (yy_buffer_end > yy_buffer_start) &&
		            ('\r' == yy_buffer[yy_buffer_end-1] ||
		             '\n' == yy_buffer[yy_buffer_end-1] ||
		             2028/*LS*/ == yy_buffer[yy_buffer_end-1] ||
		             2029/*PS*/ == yy_buffer[yy_buffer_end-1]);
	}
	private java.lang.String yytext () {
		return (new java.lang.String(yy_buffer,
			yy_buffer_start,
			yy_buffer_end - yy_buffer_start));
	}
	private int yylength () {
		return yy_buffer_end - yy_buffer_start;
	}
	private char[] yy_double (char buf[]) {
		int i;
		char newbuf[];
		newbuf = new char[2*buf.length];
		for (i = 0; i < buf.length; ++i) {
			newbuf[i] = buf[i];
		}
		return newbuf;
	}
	private final int YY_E_INTERNAL = 0;
	private final int YY_E_MATCH = 1;
	private java.lang.String yy_error_string[] = {
		"Error: Internal error.\n",
		"Error: Unmatched input.\n"
	};
	private void yy_error (int code,boolean fatal) {
		java.lang.System.out.print(yy_error_string[code]);
		java.lang.System.out.flush();
		if (fatal) {
			throw new Error("Fatal Error.\n");
		}
	}
	private int[][] unpackFromString(int size1, int size2, String st) {
		int colonIndex = -1;
		String lengthString;
		int sequenceLength = 0;
		int sequenceInteger = 0;

		int commaIndex;
		String workString;

		int res[][] = new int[size1][size2];
		for (int i= 0; i < size1; i++) {
			for (int j= 0; j < size2; j++) {
				if (sequenceLength != 0) {
					res[i][j] = sequenceInteger;
					sequenceLength--;
					continue;
				}
				commaIndex = st.indexOf(',');
				workString = (commaIndex==-1) ? st :
					st.substring(0, commaIndex);
				st = st.substring(commaIndex+1);
				colonIndex = workString.indexOf(':');
				if (colonIndex == -1) {
					res[i][j]=Integer.parseInt(workString);
					continue;
				}
				lengthString =
					workString.substring(colonIndex+1);
				sequenceLength=Integer.parseInt(lengthString);
				workString=workString.substring(0,colonIndex);
				sequenceInteger=Integer.parseInt(workString);
				res[i][j] = sequenceInteger;
				sequenceLength--;
			}
		}
		return res;
	}
	private int yy_acpt[] = {
		/* 0 */ YY_NOT_ACCEPT,
		/* 1 */ YY_NO_ANCHOR,
		/* 2 */ YY_NO_ANCHOR,
		/* 3 */ YY_NO_ANCHOR,
		/* 4 */ YY_NO_ANCHOR,
		/* 5 */ YY_NO_ANCHOR,
		/* 6 */ YY_NO_ANCHOR,
		/* 7 */ YY_NO_ANCHOR,
		/* 8 */ YY_NO_ANCHOR,
		/* 9 */ YY_NO_ANCHOR,
		/* 10 */ YY_NO_ANCHOR,
		/* 11 */ YY_NO_ANCHOR,
		/* 12 */ YY_NO_ANCHOR,
		/* 13 */ YY_NO_ANCHOR,
		/* 14 */ YY_NO_ANCHOR,
		/* 15 */ YY_NO_ANCHOR,
		/* 16 */ YY_NO_ANCHOR,
		/* 17 */ YY_NO_ANCHOR,
		/* 18 */ YY_NO_ANCHOR,
		/* 19 */ YY_NO_ANCHOR,
		/* 20 */ YY_NO_ANCHOR,
		/* 21 */ YY_NO_ANCHOR,
		/* 22 */ YY_NO_ANCHOR,
		/* 23 */ YY_NO_ANCHOR,
		/* 24 */ YY_NO_ANCHOR,
		/* 25 */ YY_NO_ANCHOR,
		/* 26 */ YY_NO_ANCHOR,
		/* 27 */ YY_NO_ANCHOR,
		/* 28 */ YY_NO_ANCHOR,
		/* 29 */ YY_NO_ANCHOR,
		/* 30 */ YY_NO_ANCHOR,
		/* 31 */ YY_NO_ANCHOR,
		/* 32 */ YY_NO_ANCHOR,
		/* 33 */ YY_NO_ANCHOR,
		/* 34 */ YY_NO_ANCHOR,
		/* 35 */ YY_NO_ANCHOR,
		/* 36 */ YY_NO_ANCHOR,
		/* 37 */ YY_NO_ANCHOR,
		/* 38 */ YY_NO_ANCHOR,
		/* 39 */ YY_NO_ANCHOR,
		/* 40 */ YY_NO_ANCHOR,
		/* 41 */ YY_NO_ANCHOR,
		/* 42 */ YY_NO_ANCHOR,
		/* 43 */ YY_NO_ANCHOR,
		/* 44 */ YY_NO_ANCHOR,
		/* 45 */ YY_NO_ANCHOR,
		/* 46 */ YY_NO_ANCHOR,
		/* 47 */ YY_NO_ANCHOR,
		/* 48 */ YY_NO_ANCHOR,
		/* 49 */ YY_NO_ANCHOR,
		/* 50 */ YY_NO_ANCHOR,
		/* 51 */ YY_NO_ANCHOR,
		/* 52 */ YY_NO_ANCHOR,
		/* 53 */ YY_NO_ANCHOR,
		/* 54 */ YY_NO_ANCHOR,
		/* 55 */ YY_NO_ANCHOR,
		/* 56 */ YY_NOT_ACCEPT,
		/* 57 */ YY_NO_ANCHOR,
		/* 58 */ YY_NO_ANCHOR,
		/* 59 */ YY_NO_ANCHOR,
		/* 60 */ YY_NOT_ACCEPT,
		/* 61 */ YY_NO_ANCHOR,
		/* 62 */ YY_NO_ANCHOR,
		/* 63 */ YY_NOT_ACCEPT,
		/* 64 */ YY_NO_ANCHOR,
		/* 65 */ YY_NOT_ACCEPT,
		/* 66 */ YY_NO_ANCHOR,
		/* 67 */ YY_NOT_ACCEPT,
		/* 68 */ YY_NO_ANCHOR,
		/* 69 */ YY_NOT_ACCEPT,
		/* 70 */ YY_NO_ANCHOR,
		/* 71 */ YY_NOT_ACCEPT,
		/* 72 */ YY_NO_ANCHOR,
		/* 73 */ YY_NOT_ACCEPT,
		/* 74 */ YY_NO_ANCHOR,
		/* 75 */ YY_NOT_ACCEPT,
		/* 76 */ YY_NO_ANCHOR,
		/* 77 */ YY_NOT_ACCEPT,
		/* 78 */ YY_NO_ANCHOR,
		/* 79 */ YY_NOT_ACCEPT,
		/* 80 */ YY_NO_ANCHOR,
		/* 81 */ YY_NO_ANCHOR,
		/* 82 */ YY_NO_ANCHOR,
		/* 83 */ YY_NO_ANCHOR,
		/* 84 */ YY_NO_ANCHOR,
		/* 85 */ YY_NO_ANCHOR,
		/* 86 */ YY_NO_ANCHOR,
		/* 87 */ YY_NO_ANCHOR,
		/* 88 */ YY_NO_ANCHOR,
		/* 89 */ YY_NO_ANCHOR,
		/* 90 */ YY_NO_ANCHOR,
		/* 91 */ YY_NO_ANCHOR,
		/* 92 */ YY_NO_ANCHOR,
		/* 93 */ YY_NO_ANCHOR,
		/* 94 */ YY_NO_ANCHOR,
		/* 95 */ YY_NO_ANCHOR,
		/* 96 */ YY_NO_ANCHOR,
		/* 97 */ YY_NO_ANCHOR,
		/* 98 */ YY_NO_ANCHOR,
		/* 99 */ YY_NO_ANCHOR,
		/* 100 */ YY_NO_ANCHOR,
		/* 101 */ YY_NO_ANCHOR,
		/* 102 */ YY_NO_ANCHOR,
		/* 103 */ YY_NO_ANCHOR,
		/* 104 */ YY_NO_ANCHOR,
		/* 105 */ YY_NO_ANCHOR,
		/* 106 */ YY_NO_ANCHOR
	};
	private int yy_cmap[] = unpackFromString(1,65538,
"41:9,44,42,41,44,42,41:18,44,8,40,7,41:3,39,1,2,6,41,13,16,43,9,38:10,10,17" +
",15,3,18,41,14,36:13,20,36:12,41,11,41:2,19,41,24,37,23,37,26,31,37,33,21,3" +
"7:2,22,32,28,27,37:2,30,25,29,35,37,34,37:3,4,12,5,41:66,36:23,41,36:7,37:2" +
"4,41,37:8,41:65280,0:2")[0];

	private int yy_rmap[] = unpackFromString(1,107,
"0,1:3,2,1:8,3,1:3,4,5,6,1:7,7:2,8,7:3,9,7:8,1:14,10,11,7,12,13,8,14,15,16,1" +
"7,18,12,19,20,21,22,23,24,25,26,27,28,29,30,31,32,8,33,34,35,36,37,38,39,40" +
",41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56")[0];

	private int yy_nxt[][] = unpackFromString(57,45,
"1,2,3,4,5,6,7,8,9,56,60,-1,10,11,12,13,-1,14,15,16,17,18,58,94,58,83,58:2,8" +
"4,95,58,85,102,58,96,58,82,58,19,20,21,-1,22,-1,22,-1:63,23,-1:42,26,-1:47," +
"82:2,57,61:14,82,61,82:2,-1:24,58:9,27,58:2,28,58:8,-1:43,19,-1:4,63,-1:20," +
"58:21,-1:24,82:2,61:15,82,61,82:2,-1:31,65,-1:11,33,-1:16,24,-1:53,82:2,61," +
"29,61:13,82,61,82:2,-1:43,59,-1:17,25,-1:52,58:13,30,58:7,-1:43,33,-1:25,58" +
":15,31,58:5,-1:21,67,-1:21,59,-1:25,58:11,32,58:9,-1:24,58:7,34,58:13,-1:5," +
"1,-1:63,58:7,35,58:13,-1:5,1,42:10,43,42:27,-1,42:5,-1:19,58:14,36,58:6,-1:" +
"5,1,44:27,45,46,44:12,-1,44:2,-1:19,58:3,37,58:17,-1:5,1,-1:38,47,-1:24,58:" +
"7,38,58:13,-1:5,1,48:10,49,48:28,50,48,-1,48:2,-1:19,58:14,39,58:6,-1:5,1,5" +
"1:10,52,51:16,53,54,51:10,55,51,-1,51:2,-1:19,58:10,40,58:10,-1:24,58:10,41" +
",58:10,-1:24,58:7,103,58:8,62,58:4,-1:24,58:7,64,58:13,-1:24,58:5,98,58:2,6" +
"6,58:12,-1:24,58:6,68,58:14,-1:24,58:16,70,58:4,-1:24,58:10,72,58:10,-1:24," +
"58:5,74,58:15,-1:24,58:6,76,58:14,-1:24,58:4,78,58:16,-1:24,58:4,80,58:16,-" +
"1:24,58:4,81,58:16,-1:24,58:5,86,58:2,106,58:12,-1:24,58:8,97,58:2,87,58:9," +
"-1:24,58:2,88,58:18,-1:24,58:10,89,58:10,-1:24,58:3,90,58:17,-1:24,58:10,91" +
",58:10,-1:24,58:7,92,58:13,-1:24,58:5,93,58:15,-1:24,58:5,99,58:15,-1:24,58" +
":3,100,58:17,-1:24,58:11,101,58:9,-1:24,58:10,104,58:10,-1:24,58:9,105,58:1" +
"1,-1:5");

	public java_cup.runtime.Symbol next_token ()
		throws java.io.IOException {
		int yy_lookahead;
		int yy_anchor = YY_NO_ANCHOR;
		int yy_state = yy_state_dtrans[yy_lexical_state];
		int yy_next_state = YY_NO_STATE;
		int yy_last_accept_state = YY_NO_STATE;
		boolean yy_initial = true;
		int yy_this_accept;

		yy_mark_start();
		yy_this_accept = yy_acpt[yy_state];
		if (YY_NOT_ACCEPT != yy_this_accept) {
			yy_last_accept_state = yy_state;
			yy_mark_end();
		}
		while (true) {
			if (yy_initial && yy_at_bol) yy_lookahead = YY_BOL;
			else yy_lookahead = yy_advance();
			yy_next_state = YY_F;
			yy_next_state = yy_nxt[yy_rmap[yy_state]][yy_cmap[yy_lookahead]];
			if (YY_EOF == yy_lookahead && true == yy_initial) {
				return null;
			}
			if (YY_F != yy_next_state) {
				yy_state = yy_next_state;
				yy_initial = false;
				yy_this_accept = yy_acpt[yy_state];
				if (YY_NOT_ACCEPT != yy_this_accept) {
					yy_last_accept_state = yy_state;
					yy_mark_end();
				}
			}
			else {
				if (YY_NO_STATE == yy_last_accept_state) {
					throw (new Error("Lexical Error: Unmatched Input."));
				}
				else {
					yy_anchor = yy_acpt[yy_last_accept_state];
					if (0 != (YY_END & yy_anchor)) {
						yy_move_end();
					}
					yy_to_mark();
					switch (yy_last_accept_state) {
					case 1:
						
					case -2:
						break;
					case 2:
						{ return new Symbol(sym._SYMB_0); }
					case -3:
						break;
					case 3:
						{ return new Symbol(sym._SYMB_1); }
					case -4:
						break;
					case 4:
						{ return new Symbol(sym._SYMB_2); }
					case -5:
						break;
					case 5:
						{ return new Symbol(sym._SYMB_3); }
					case -6:
						break;
					case 6:
						{ return new Symbol(sym._SYMB_4); }
					case -7:
						break;
					case 7:
						{ return new Symbol(sym._SYMB_5); }
					case -8:
						break;
					case 8:
						{ return new Symbol(sym._SYMB_6); }
					case -9:
						break;
					case 9:
						{ return new Symbol(sym._SYMB_7); }
					case -10:
						break;
					case 10:
						{ return new Symbol(sym._SYMB_10); }
					case -11:
						break;
					case 11:
						{ return new Symbol(sym._SYMB_11); }
					case -12:
						break;
					case 12:
						{ return new Symbol(sym._SYMB_12); }
					case -13:
						break;
					case 13:
						{ return new Symbol(sym._SYMB_16); }
					case -14:
						break;
					case 14:
						{ return new Symbol(sym._SYMB_14); }
					case -15:
						break;
					case 15:
						{ return new Symbol(sym._SYMB_17); }
					case -16:
						break;
					case 16:
						{ return new Symbol(sym._SYMB_18); }
					case -17:
						break;
					case 17:
						{ return new Symbol(sym.Name, yytext().intern()); }
					case -18:
						break;
					case 18:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -19:
						break;
					case 19:
						{ return new Symbol(sym._INTEGER_, new Integer(yytext())); }
					case -20:
						break;
					case 20:
						{ yybegin(CHAR); }
					case -21:
						break;
					case 21:
						{ yybegin(STRING); }
					case -22:
						break;
					case 22:
						{ /* ignore white space. */ }
					case -23:
						break;
					case 23:
						{ return new Symbol(sym._SYMB_15); }
					case -24:
						break;
					case 24:
						{ return new Symbol(sym._SYMB_8); }
					case -25:
						break;
					case 25:
						{ return new Symbol(sym._SYMB_9); }
					case -26:
						break;
					case 26:
						{ return new Symbol(sym._SYMB_13); }
					case -27:
						break;
					case 27:
						{ return new Symbol(sym._SYMB_25); }
					case -28:
						break;
					case 28:
						{ return new Symbol(sym._SYMB_24); }
					case -29:
						break;
					case 29:
						{ return new Symbol(sym._SYMB_19); }
					case -30:
						break;
					case 30:
						{ return new Symbol(sym._SYMB_29); }
					case -31:
						break;
					case 31:
						{ return new Symbol(sym._SYMB_27); }
					case -32:
						break;
					case 32:
						{ return new Symbol(sym._SYMB_23); }
					case -33:
						break;
					case 33:
						{ return new Symbol(sym._DOUBLE_, new Double(yytext())); }
					case -34:
						break;
					case 34:
						{ return new Symbol(sym._SYMB_20); }
					case -35:
						break;
					case 35:
						{ return new Symbol(sym._SYMB_31); }
					case -36:
						break;
					case 36:
						{ return new Symbol(sym._SYMB_32); }
					case -37:
						break;
					case 37:
						{ return new Symbol(sym._SYMB_30); }
					case -38:
						break;
					case 38:
						{ return new Symbol(sym._SYMB_22); }
					case -39:
						break;
					case 39:
						{ return new Symbol(sym._SYMB_26); }
					case -40:
						break;
					case 40:
						{ return new Symbol(sym._SYMB_28); }
					case -41:
						break;
					case 41:
						{ return new Symbol(sym._SYMB_21); }
					case -42:
						break;
					case 42:
						{ yybegin(CHAREND); return new Symbol(sym._CHAR_, new Character(yytext().charAt(0))); }
					case -43:
						break;
					case 43:
						{ yybegin(CHARESC); }
					case -44:
						break;
					case 44:
						{ yybegin(CHAREND); return new Symbol(sym._CHAR_, new Character(yytext().charAt(0))); }
					case -45:
						break;
					case 45:
						{ yybegin(CHAREND); return new Symbol(sym._CHAR_, new Character('\n')); }
					case -46:
						break;
					case 46:
						{ yybegin(CHAREND); return new Symbol(sym._CHAR_, new Character('\t')); }
					case -47:
						break;
					case 47:
						{yybegin(YYINITIAL);}
					case -48:
						break;
					case 48:
						{ pstring += yytext(); }
					case -49:
						break;
					case 49:
						{ yybegin(ESCAPED); }
					case -50:
						break;
					case 50:
						{ String foo = pstring; pstring = new String(); yybegin(YYINITIAL); return new Symbol(sym._STRING_, foo.intern()); }
					case -51:
						break;
					case 51:
						{ pstring += yytext(); yybegin(STRING); }
					case -52:
						break;
					case 52:
						{ pstring += "\\"; yybegin(STRING); }
					case -53:
						break;
					case 53:
						{ pstring +=  "\n"; yybegin(STRING); }
					case -54:
						break;
					case 54:
						{ pstring += "\t"; yybegin(STRING); }
					case -55:
						break;
					case 55:
						{ pstring += "\""; yybegin(STRING); }
					case -56:
						break;
					case 57:
						{ return new Symbol(sym.Name, yytext().intern()); }
					case -57:
						break;
					case 58:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -58:
						break;
					case 59:
						{ return new Symbol(sym._DOUBLE_, new Double(yytext())); }
					case -59:
						break;
					case 61:
						{ return new Symbol(sym.Name, yytext().intern()); }
					case -60:
						break;
					case 62:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -61:
						break;
					case 64:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -62:
						break;
					case 66:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -63:
						break;
					case 68:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -64:
						break;
					case 70:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -65:
						break;
					case 72:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -66:
						break;
					case 74:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -67:
						break;
					case 76:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -68:
						break;
					case 78:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -69:
						break;
					case 80:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -70:
						break;
					case 81:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -71:
						break;
					case 82:
						{ return new Symbol(sym.Name, yytext().intern()); }
					case -72:
						break;
					case 83:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -73:
						break;
					case 84:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -74:
						break;
					case 85:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -75:
						break;
					case 86:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -76:
						break;
					case 87:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -77:
						break;
					case 88:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -78:
						break;
					case 89:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -79:
						break;
					case 90:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -80:
						break;
					case 91:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -81:
						break;
					case 92:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -82:
						break;
					case 93:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -83:
						break;
					case 94:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -84:
						break;
					case 95:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -85:
						break;
					case 96:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -86:
						break;
					case 97:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -87:
						break;
					case 98:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -88:
						break;
					case 99:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -89:
						break;
					case 100:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -90:
						break;
					case 101:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -91:
						break;
					case 102:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -92:
						break;
					case 103:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -93:
						break;
					case 104:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -94:
						break;
					case 105:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -95:
						break;
					case 106:
						{ return new Symbol(sym.Var, yytext().intern()); }
					case -96:
						break;
					default:
						yy_error(YY_E_INTERNAL,false);
					case -1:
					}
					yy_initial = true;
					yy_state = yy_state_dtrans[yy_lexical_state];
					yy_next_state = YY_NO_STATE;
					yy_last_accept_state = YY_NO_STATE;
					yy_mark_start();
					yy_this_accept = yy_acpt[yy_state];
					if (YY_NOT_ACCEPT != yy_this_accept) {
						yy_last_accept_state = yy_state;
						yy_mark_end();
					}
				}
			}
		}
	}
}
