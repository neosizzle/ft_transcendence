npx prisma generate
npx prisma db push

npm run build
bash service/userStatus.sh &
npm run start:prod