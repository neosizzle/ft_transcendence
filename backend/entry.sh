bash service/userStatus.sh &
npm install -g webpack --legacy-peer-deps  &&
npx prisma generate &&
npm run build &&
npm run start:prod
# tail -f