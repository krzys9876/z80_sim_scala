echo off
cls
java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.4.jar input-files\basicall_KR_simpleIO.hex
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.4.jar input-files\basicall_KR_simpleIO.hex input-files\waves.txt 250000000
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.4.jar input-files\basicall_KR_simpleIO.hex input-files\tictactoe10.txt 3600000000
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.4.jar input-files\basicall_KR_simpleIO.hex input-files\input.txt 450000
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.4.jar input-files\basicall_KR_simpleIO.hex input-files\arithmetic.txt 1400000