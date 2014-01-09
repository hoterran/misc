public class J {

	static void foo1(int x) {
		x = 1;	
	}

	static void foo3(String s) {
		s = new String("def");	
	}

	static void foo4(StringBuffer s) {
		s.append("def");	
	}

	public static void main(String[] args) {
		int x = 0;
		foo1(x);
		System.out.println(x);
		String s = new String("abc");
		foo3(s);
		System.out.println(s);

		StringBuffer s1 = new StringBuffer("abc");
		foo4(s1);
		System.out.println(s1);
	}
}
