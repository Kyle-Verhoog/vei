// TYPE_LINKING
/**
 * TypeLinking:
 * - All classes implicitly import java.lang.*, even in the presence
 * of other import-on-demand declarations.  
 */
import java.lang.ref.*;

public class Main {
    
    public Main(){}

    public static int test(int i){
	return new Integer(123).intValue();
    }

    public static void temp() {
        if (1 > 3) {
        } else {

        }


        if (1 < 10) {
            int x = 0;
            if (1 != 4) {
                x = 2;
            }
        } else if (2 == 4) {
            int y = 3;
        } else if (2 >= 4) {
            int y = 3;
        } else {
            int y = 0;
        }
    }
}
