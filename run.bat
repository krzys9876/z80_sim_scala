echo off
cls
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.3.jar input-files\basicall_KR_simpleIO_02.hex
java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.3.jar input-files\basicall_KR_simpleIO_02.hex input-files\waves.txt 250000000
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.3.jar input-files\basicall_KR_simpleIO_02.hex input-files\tictactoe10.txt 3600000000
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.3.jar input-files\basicall_KR_simpleIO_02.hex input-files\input.txt 450000
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.3.jar input-files\basicall_KR_simpleIO_02.hex input-files\arithmetic.txt 1400000