DURATION=30

while true
do
	printf ' Checking for inactive users...\n'
	node service/userStatus.js
	sleep $DURATION
done