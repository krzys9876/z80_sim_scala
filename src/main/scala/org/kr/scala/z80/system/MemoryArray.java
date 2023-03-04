package org.kr.scala.z80.system;

public class MemoryArray {
    int[] data;
    public MemoryArray(int size,int[] initial) {
        data=new int[size];
        System.arraycopy(initial, 0, data, 0, initial.length);
    }
}
