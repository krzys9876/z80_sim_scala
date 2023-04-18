package org.kr.scala.z80.system;

public class RegisterArray {
    int[] data;
    public RegisterArray(int size, int[] initial) {
        data=new int[size];
        System.arraycopy(initial, 0, data, 0, initial.length);
    }
}
