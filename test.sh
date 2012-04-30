#! /bin/bash
# Run all test suite and print results
echo "";
echo "Testing all programs in the test suite";
cd testsuite;
for folder in should_compile should_fail; do
	for file in $folder/*.lla ; do  
   		echo "-------------------------------"
  		echo "Testing $file";
   		../src/parser/llama $file;
   		OUT=$?
   		if [ $OUT -eq 0 ];then
      		echo "Program compiled successfully."
   		else
      		echo "Compilation failed."
   		fi
	done
done
echo "-------------------------------";
echo "";
