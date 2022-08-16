// eslint-disable-next-line @typescript-eslint/no-var-requires, no-undef
const {PrismaClient} = require("@prisma/client")

const prisma = new PrismaClient();

const main = async () => 
{
	const data = await prisma.auth.findMany({
		where : {
			expiresAt : {
				lt : new Date()
			}
		}
	});
	data.forEach(async authUser => {
		const user = await prisma.user.findUnique({where : {id : authUser.userId}})
		if (authUser.expiresAt < new Date() && (user.status === "LOGGEDIN" || user.status === "INGAME"))
		{
			const setOfflineRes = await prisma.user.update({
				where : {
					id : authUser.userId
				},
				data : {
					status : "OFFLINE"
				}
			})
			console.log("set " + setOfflineRes.username + " status to offline due to inactivity..")
			await prisma.auth.delete({where : {id : authUser.id}})
		}
	})
}

main();