/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler.analysis

import openisetl.compiler.parser.DeepVisitorAdapter

class LocAn extends DeepVisitorAdapter {

}
/*
 *
public class LocAn extends DepthFirstAdapter {

	TreeSet<Token> tks = new TreeSet<Token>(new Comparator<Token>() {

		public int compare(Token a, Token b) {
			int linea = a.getLine(), lineb = b.getLine();
			if (linea == lineb) {
				int cola = a.getPos(), colb = b.getPos();
				if (cola > colb)
					return 1;
				if (cola < colb)
					return -1;
				return 0;
			} else if (linea > lineb) {
				return 1;
			} else {
				return -1;
			}
		}
	});

	private LocAn() {

	}

	public static Token getToken(Node n) {

		LocAn an = new LocAn();
		n.apply(an);

		if (an.tks.size() < 1) {
			throw new NoSuchElementException();
		}

		return an.tks.first();
	}

	public static Location getLoc(Node n) {

		LocAn an = new LocAn();
		n.apply(an);

		if (an.tks.size() < 1) {
			return Location.NullLoc;
		}

		Token ftk = an.tks.first();
		Token ltk = an.tks.last();
		
		return Location.intern(XLexer.getProgramFile(ftk), ftk.getLine(), ftk
				.getPos(), ltk.getLine(), ltk.getPos() + ltk.getText().length()
				- 1);
	}

	@Override
	public void defaultCase(Node node) {
		if (node instanceof Token) {
			tks.add((Token) node);
		}
	}

}

*/