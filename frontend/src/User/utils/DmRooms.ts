import { intersectionBy } from "lodash";
import { Member, Room } from "../../Chat/classes";
import { API_ROOT, OUTGOING_CREATE } from "../../constants";
import { AuthCtx, User } from "../../context/authContext";
import { auth_net_get } from "../../utils";

const membersEndpoint = `${API_ROOT}/members`;

// creats a new dm room with user
export const createNewDm = (auth: AuthCtx, user: User) => {
  return new Promise<Room>((resolve) => {
    auth?.chatSocket?.emit(
      OUTGOING_CREATE,
      JSON.stringify({ type: "DM", initialUsers: `${user.id}` }),
      (room: Room) => resolve(room)
    );
  });
};

// gets common dm room between two users
// TODO : unauthorized error handling
export const getCommonDmRoom = async (
  auth: AuthCtx,
  user1: User,
  user2: User
) => {
  const myMembers = await auth_net_get(
    `${membersEndpoint}?page=1&pageSize=1&filterOn=userId&filterBy=${user1.id}`
  );
  const theirMembers = await auth_net_get(
    `${membersEndpoint}?page=1&pageSize=1&filterOn=userId&filterBy=${user2.id}`
  );
  const myTotalElements = myMembers.total_elements;
  const theirTotalElements = theirMembers.total_elements;
  if (!myTotalElements || !theirTotalElements) {
    const data = await createNewDm(auth, user2);
    if (data.id) return data;
    else return undefined;
  }
  const myMembersData = await auth_net_get(
    `${membersEndpoint}?page=1&pageSize=${myTotalElements}&filterOn=userId&filterBy=${user1.id}`
  );
  const theirMembersData = await auth_net_get(
    `${membersEndpoint}?page=1&pageSize=${theirTotalElements}&filterOn=userId&filterBy=${user2.id}`
  );

  const myMembersList: Member[] = myMembersData.data;
  const theirMembersList: Member[] = theirMembersData.data;
  const commonMembers: Member[] = intersectionBy(
    theirMembersList,
    myMembersList,
    "roomId"
  );

  const commonDMMember = commonMembers.find(
    (member) => member.room.type === "DM"
  );
  return commonDMMember?.room;
};
