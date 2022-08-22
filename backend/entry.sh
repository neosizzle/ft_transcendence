npx prisma generate && npx prisma db push &&
bash service/userStatus.sh &
npm run build &&
npm run start:prod
# tail -f