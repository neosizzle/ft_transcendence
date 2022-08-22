bash service/userStatus.sh &
npm install -g webpack --legacy-peer-deps  &&
npx prisma generate && npx prisma db push &&
npm run build &&
npm run start:prod
# tail -f