npx prisma generate && npx prisma db push &&
npm link webpack &&
bash service/userStatus.sh &
npm run build &&
npm run start:prod
# tail -f