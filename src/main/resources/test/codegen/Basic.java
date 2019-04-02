// CODE_GENERATION
public class Basic {

    public String s;

    public Basic (String str) {
        this.s = str+str+str;
    }

    public static int test() {
        String str = "111" +  2;
        System.out.println(str);
        return 0;
        // if (Integer.parseInt("4"+"2")==42)
        //     str = str+"22";
        // else
        //     str = str+"33";
        // int i=0;
        // while (i<10) {
        //     i = i+1;
        //     str = str+"444";
        // }
        // for (int j=0; j<10; j=j+1) {
        //     str = str+"55"+"66"+str;
        // }
        // if (new Basic(str).s.equals((Object)(str+str+str)))
        //     return 123;
        // else
        //     return 0;
    }

}
