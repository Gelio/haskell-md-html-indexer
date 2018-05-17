echo off

set /a number=%random% %% 10
echo %1 waiting %number% seconds
sleep %number%
echo %1 finished
