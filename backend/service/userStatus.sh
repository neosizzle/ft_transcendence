DURATION=30

while true
do
	printf '[USER STATUS SERVICE] 🖨️ Checking for inactive users...\n'
	node service/userStatus.js
	sleep $DURATION
done