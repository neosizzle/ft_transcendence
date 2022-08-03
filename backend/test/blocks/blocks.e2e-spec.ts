const runBlocksTests = (pactum: any) => {
  describe("Blocks", () => {
    it(`/blocks (POST) bad dto`, () => {
      const dto = {
        intraName: "wtf",
      };

      return pactum
        .spec()
        .post(`/blocks`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(404)
        .expectBodyContains("error");
    });

    it(`/blocks (POST) bad dto 2`, () => {
      const dto = {};

      return pactum
        .spec()
        .post(`/blocks`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it(`/blocks (POST) bad dto 3`, () => {
      const dto = {
        intraName: "test",
        id: "69",
      };

      return pactum
        .spec()
        .post(`/blocks`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it(`/blocks (POST) bad dto 4`, () => {
      const dto = {
        id: "0",
      };

      return pactum
        .spec()
        .post(`/blocks`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it(`/blocks (POST)`, () => {
      const dto = {
        intraName: "test",
      };

      return pactum
        .spec()
        .post(`/blocks`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(201);
    });

    it(`/blocks (POST) 2`, () => {
      const dto = {
        intraName: "tes",
      };

      return pactum
        .spec()
        .post(`/blocks`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(201);
    });

    it(`/blocks (POST) 3`, () => {
      const dto = {
        intraName: "test2",
      };

      return pactum
        .spec()
        .post(`/blocks`)
        .withBody(dto)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(201);
    });

    it(`/blocks (GET) no pgination`, () => {
      return pactum
        .spec()
        .get(`/blocks`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(400)
        .expectBodyContains("error");
    });

    it(`/blocks (GET) pagination`, () => {
      return pactum
        .spec()
        .get(`/blocks?page=1&pageSize=3`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectBodyContains("tes")
        .expectBodyContains("test");
    });

    it(`/blocks (GET) filtering`, () => {
      return pactum
        .spec()
        .get(
          `/blocks?page=1&pageSize=30&filterOn=avatar,status&filterBy=asdf.avatar.jpg,OFFLINE`
        )
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonLength("data", 2)
        .stores("blockid", "data[0].blockee.id");
    });

    it(`/blocks (DELETE) no effect`, () => {
      return pactum
        .spec()
        .delete(`/blocks/0`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonMatch({ count: 0 });
    });

    it(`/blocks (DELETE) deleted`, () => {
      return pactum
        .spec()
        .delete(`/blocks/$S{blockid}`)
        .withHeaders({
          Authorization: "Bearer $S{token}",
        })
        .expectStatus(200)
        .expectJsonMatch({ count: 1 });
    });
  });
};

export default runBlocksTests;
