echo off
cls
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode interactive --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\waves.txt" --memory-type slow
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode interactive --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\waves.txt" --memory-type fast --register-type fast
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\waves.txt" --memory-type fast --steps-m 250
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode interactive --hex-file input-files\basicall_KR_simpleIO.hex --basic-file input-files\tictactoe10.txt
rem java.exe -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode interactive --hex-file input-files\basicall_KR_simpleIO.hex --basic-file input-files\tictactoe10.txt --steps-m 3600 --memory-type fast --register-type fast
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file input-files\basicall_KR_simpleIO.hex --basic-file input-files\input.txt --memory-type fast --steps-m 0.45

rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic.txt" --steps-m 1.4 --memory-type slow --register-type slow
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic.txt" --steps-m 1.4 --memory-type fast --register-type slow
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic.txt" --steps-m 1.4 --memory-type slow --register-type fast
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic.txt" --steps-m 1.4 --memory-type fast --register-type fast

rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic2.txt" --steps-m 14 --memory-type fast --register-type fast
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic2.txt" --steps-m 14 --memory-type fast --register-type slow
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic2.txt" --steps-m 14 --memory-type slow --register-type fast
rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic2.txt" --steps-m 14 --memory-type slow --register-type slow

java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.14.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic3.txt" --steps-m 50 --memory-type fast --register-type fast
java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.14.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic3.txt" --steps-m 50 --memory-type fast --register-type slow
java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.14.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic3.txt" --steps-m 50 --memory-type slow --register-type fast
java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.14.jar --mode batch --hex-file "input-files\basicall_KR_simpleIO.hex" --basic-file "input-files\arithmetic3.txt" --steps-m 50 --memory-type slow --register-type slow

rem java.exe -XX:+UseSerialGC -jar target\scala-2.13\z80_sim-assembly-0.0.13.jar --mode interactive --hex-file "input-files\basicall_KR_simpleIO.hex" --memory-type fast
