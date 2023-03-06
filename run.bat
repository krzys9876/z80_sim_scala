echo off
cls
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.5.jar --mode interactive --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\waves.txt" --memory-type slow
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.5.jar --mode interactive --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\waves.txt" --memory-type fast
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.5.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\waves.txt" --steps-m 250
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.5.jar --mode interactive --hex-file input-files\basicall_KR_simpleIO.hex --basic-file input-files\tictactoe10.txt
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.5.jar --mode interactive --hex-file input-files\basicall_KR_simpleIO.hex --basic-file input-files\tictactoe10.txt --steps-m 3600 --memory-type fast
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.5.jar --mode batch --hex-file input-files\basicall_KR_simpleIO.hex --basic-file input-files\input.txt --steps-m 0.45
java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.5.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic.txt" --steps-m 1.4 --memory-type fast
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.5.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic2.txt" --steps-m 1.4 --memory-type fast
