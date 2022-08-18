import { INestApplication, ValidationPipe } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { AppModule } from "src/app.module";
import { PrismaService } from "src/prisma/prisma.service";

const runAdminTests = (pactum: any) => {
  let app: INestApplication;
  let prisma: PrismaService;

  //run this before all testcases
  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();
    app = moduleRef.createNestApplication();
    app.useGlobalPipes(
      new ValidationPipe({
        whitelist: true,
      })
    );
    await app.init();
    prisma = app.get(PrismaService);
  });

  describe("Admins", () => {
    it("/admins (POST) bad dto", () => {
      const dto = {
        intraName: "wtf",
      };

      return pactum
        .spec()
        .post(`/admins`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/admins (POST) bad dto 2", () => {
      const dto = {
        roomId: -1,
      };

      return pactum
        .spec()
        .post(`/admins`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/admins (POST) DM", async () => {
      const room = await prisma.room.findFirst({
        where: {
          type: "DM",
          owner: {
            username: "new",
          },
        },
      });
      const dto = {
        roomId: room.id,
        userId: room.ownerId,
      };

      return pactum
        .spec()
        .post(`/admins`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/admins (POST) no permission", async () => {
      const room = await prisma.room.findFirst({
        where: {
          type: "GC",
        },
      });
      const user = await prisma.user.findFirst({
        where: { username: "new" },
      });
      
      await prisma.room.update({
        where: { id: room.id },
        data: { ownerId: user.id },
      });
      const dto = {
        roomId: room.id,
        userId: user.id,
      };

      return pactum
        .spec()
        .post(`/admins`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/admins (POST) room not found", async () => {
      const user = await prisma.user.findFirst({
        where: { username: "new" },
      });
      const dto = {
        roomId: -123,
        userId: user.id,
      };

      return pactum
        .spec()
        .post(`/admins`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(404)
        .expectBodyContains("error");
    });

    it("/admins (POST) transfer ownership and promote", async () => {
      const user = await prisma.user.findFirst({
        where: { username: "new" },
      });
      const room = await prisma.room.findFirst({ where: { type: "GC" } });
      await prisma.room.update({
        where: { id: room.id },
        data: { ownerId: user.id },
      });

      //transfer ownership dont demote admin, so i have to demote manually
      const userToPromote = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      await prisma.admin.deleteMany({
        where: { userId: userToPromote.id, roomId: room.id },
      });

      const dto = {
        roomId: room.id,
        userId: userToPromote.id,
      };

      return pactum
        .spec()
        .post(`/admins`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(201);
    });

    it("/admins (POST) promote existing admin", async () => {
      const room = await prisma.room.findFirst({ where: { type: "GC" } });
      const userToPromote = await prisma.user.findFirst({
        where: { username: "test2" },
      });

      const dto = {
        roomId: room.id,
        userId: userToPromote.id,
      };

      return pactum
        .spec()
        .post(`/admins`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/admins (POST) promote non member", async () => {
      const room = await prisma.room.findFirst({ where: { type: "GC" } });
      const userToPromote = await prisma.user.findFirst({
        where: { username: "test" },
      });

      const dto = {
        roomId: room.id,
        userId: userToPromote.id,
      };

      return pactum
        .spec()
        .post(`/admins`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(404)
        .expectBodyContains("error");
    });

    it(`/admins (GET) no pagination`, () => {
      return pactum
        .spec()
        .get(`/admins`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it(`/admins (GET) pagination`, () => {
      return pactum
        .spec()
        .get(`/admins?page=1&pageSize=3`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonLength("data", 2);
    });

    it(`/amdins (GET) filtering bad`, () => {
      return pactum
        .spec()
        .get(
          `/admins?page=1&pageSize=3&filterOn=avatar,status&filterBy=asdf.avatar.jpg,OFFLINE`
        )
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400);
    });

    it(`/admins (GET) filtering`, () => {
      return pactum
        .spec()
        .get(`/admins?page=1&pageSize=3&filterOn=roomId,userId&filterBy=-1,-1`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonLength("data", 0);
    });

    it(`/admins (GET) filtering 2`, async () => {
      const user = await prisma.user.findFirst({
        where: {
          username: "new",
        },
      });
      return pactum
        .spec()
        .get(`/admins?page=1&pageSize=3&filterOn=userId&filterBy=${user.id}`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonLength("data", 1);
    });

    it("/admins (DELETE) demote self", async () => {
      const user = await prisma.user.findFirst({
        where: { username: "new" },
      });
      const adminToDemote = await prisma.admin.findFirst({
        where: {
          userId: user.id,
        },
      });
      return pactum
        .spec()
        .delete(`/admins/${adminToDemote.id}`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it("/admins (DELETE) bad id", async () => {
      return pactum
        .spec()
        .delete(`/admins/-1`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(404)
        .expectBodyContains("error");
    });

    it("/admins (DELETE) demote self", async () => {
      const user = await prisma.user.findFirst({
        where: { username: "test2" },
      });
      const adminToDemote = await prisma.admin.findFirst({
        where: {
          userId: user.id,
        },
      });
      return pactum
        .spec()
        .delete(`/admins/${adminToDemote.id}`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200);
    });
  });
};

export default runAdminTests;
