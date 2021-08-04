@REM @echo off
cd /d %~dp0/..
docker build -t ratlas-test -f tests\tests_env\Dockerfile . && docker run -it ratlas-test bash
