public class Bootstrap {
    static {
        System.loadLibrary("fjni_core");
    }
    
    public static native int addNumbers(int a, int b);
    
    public static void main(String[] args) {
        int result = addNumbers(3, 4);
        System.out.println("Fortran says: " + result);
    }
}
