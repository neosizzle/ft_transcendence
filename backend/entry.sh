bash service/userStatus.sh &
npm install --save-dev webpack --legacy-peer-deps  &&
npx prisma generate &&
npm run build &&
npm run start:prod
# tail -f